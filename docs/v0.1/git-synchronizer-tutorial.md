# Git repository synchronizer

We will build a CLI application that keeps track of file modifications using the iNotify UNIX API, it will also perform git commits whenever a file changes and also syncronizes (pull/push) with remote repository on intervals of time.

For this tutorial, we assume the reader is familiar with:

* [Stack](https://docs.haskellstack.org/en/stable/README/) projects
* [GHC Extensions](https://downloads.haskell.org/~ghc/8.2.2/docs/html/users_guide/glasgow_exts.html)
* [Shelly library](https://hackage.haskell.org/package/shelly)
* [Haskell concurrency](http://chimera.labs.oreilly.com/books/1230000000929/pt02.html) (threads and STM)

If you are not familiar with the topics above, we recommend reading the tutorial while looking information from the given links.

## What you'll learn:

* How to create supervision trees using Capataz
* How to use the `ComponentM` monad from the [teardown library](https://stackage.org/packages/teardown)
* How to use the shelly library to run commands
* How to use RIO for logging

## Summary of what our program will do

Our CLI program will receive various strings as CLI arguments, each argument represents a path that points to a git repository which we want to automatically synchronize with a remote repository; by just saving a file a commit should be created and the repository should be synchronized often. We will have a supervisor for each repository, where each of them contain various workers to accomplish our tasks.

You can find the code for this tutorial in the [`examples`](https://github.com/roman/Haskell-capataz/tree/master/examples/) directory of the project's Github repository.

## Setting up the stage - Modeling our domain problem as a supervisor tree

We first define a diagram of how our supervision tree is going to look like:

```text
capataz-system/
├── logger-worker  -- ^ performs logging to a console
└── repository-supervisor(*)
    ├── git-worker            -- ^ performs all git operations on repository
    ├── repo-file-watcher     -- ^ monitors changes on files
    └── sync-interval-worker  -- ^ notifies every interval of time an event
```

Our Capataz' root supervisor tree is composed by a worker green thread reponsible of logging, and one (1) to many `repository-supervisor` supervisor threads that monitor "internal" workers. Often, every branch on a supervision tree node is an intermediate supervisor that monitors workers that collaborate to achieve a goal.

## How do our workers communicate?

Given Capataz does not enforce a message passing communication scheme, we can use whatever we want to communicate our workers, from third party services like SQS, to external servers like RabbitMQ or Redis, to memory STM Queues and MVars.

In this example we are going to use `STM` given is ok for our messages to be dropped in case of catastrophic failure.

## Implementing our supervision tree

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib where

import qualified Prelude
import RIO  -- (1)
import qualified RIO.Logger as Logger

import Control.Monad.Component (ComponentM)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Capataz (Capataz, ProcessSpec)
import Control.Concurrent.STM (orElse)

import System.Environment (getArgs)

import qualified RIO.Text as Text

import qualified Control.Monad.Component as Component
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.Capataz as Capataz

import qualified Shelly -- (1)
import qualified System.INotify as INotify -- (1)

```

`(1)` For this example we use [`RIO`](https://github.com/commercialhaskell/rio) as our custom Prelude, we use [`shelly`](https://hackage.org/package/shelly) to execute CLI commands from Haskell, and we use [`hinotify`](https://hackage.org/hinotify) to keep track of files changes.

```haskell
--------------------------------------------------------------------------------

-- | An instance of HasLogFunc for a LogFunc, I create this instance given
-- there is no obvious way to use a LogFunc in a reader. I'm confident there
-- must be a better way to do this. For the sake of the tutorial, this is a
-- non-essential implementation detail
instance Logger.HasLogFunc Logger.LogFunc where
  logFuncL = id

-- | Utility function that should be included in the teardown library, no need
-- to understand what is this used for.
withComponent :: Text -> ComponentM a -> (a -> IO ()) -> IO ()
withComponent !appDesc !buildComponent !f = mask $ \restore -> do
  component <- Component.runComponentM appDesc buildComponent
  (restore . f $ Component.fromComponent component)
      `finally`
      (Component.teardown component >>= Prelude.print)

--------------------------------------------------------------------------------

-- | A combination of both file modification events and git repository
-- synchronization.
data WatcherMsg -- (2)
  -- | Message triggered when a monitored file changes
  = FileChanged !FilePath
  -- | Message triggered when a sync request is made
  | SyncRequested
```

`(2)` We use a record to trigger events for file changes registered by `hinotify` and
a repository synchronization request.

```haskell
--------------------------------------------------------------------------------

-- | Returns an INotify descriptor, necessary to build watches for a directory
buildINotify :: ComponentM INotify.INotify
buildINotify = Component.buildComponentWithCleanup $ do
  inotify <- INotify.initINotify
  return (inotify, ("inotify descriptor", INotify.killINotify inotify))

-- | Returns an STM sub-routine that returns a filepath that has been modified,
-- this sub-routine retries until such change happens in the filesystem.
buildFileWatcher
  :: INotify.INotify
  -> FilePath -- ^ Directory where changes are tracked
  -> ComponentM (STM FilePath)
buildFileWatcher inotify !dir =
  -- (3)
  Component.buildComponentWithCleanup $ mask $ \_ -> do
    fileChangesChan <- TChan.newTChanIO -- (4)
    -- (5)
    fileWatch <- INotify.addWatch inotify [INotify.CloseWrite, INotify.Modify] dir $ \ev -> do
      case ev of
        INotify.Modified {INotify.isDirectory, INotify.maybeFilePath}
          -- we ignore all changes that happen to a Directory
        | isDirectory -> return ()
        | otherwise ->
            maybe (return ())
                  (atomically . TChan.writeTChan fileChangesChan)
                  maybeFilePath
        _ -> return ()

    -- (6)
    return ( TChan.readTChan fileChangesChan
           , ("inotify:" <> Text.pack dir, INotify.removeWatch fileWatch)
           )
```

`(3)` The `Component.buildComponentWithCleanup` allows us to allocate resources and then return a value from the allocation and a named teardown sub-routine that gets composed automatically by the `ComponentM` monad.

`(4)` We use a `TChan` to communicate to other interested threads that a file notification has been tracked by `hinotify`.

`(5)` Using the `INotify.addWatch` function, we can register that we are interested in receiving events on file modification, and we provide a callback that writes those events a `TChan` that we created in step `(4)`

`(6)` We return both an `STM` sub-routine that blocks until a `FilePath` is emitted, and a cleanup strategy that gets merged with other cleanup strategies via the `ComponentM` monad.

```haskell

-- | Returns both an STM sub-routine that blocks until a given period of time
-- has passed, and a "ProcessSpec" for supervision of this interval thread.
buildIntervalWorker
  :: Text  -- ^ Name of the worker process
  -> Int   -- ^ Number of seconds between notifications
  -> ComponentM (STM (), ProcessSpec)
buildIntervalWorker !workerName !delaySeconds = Component.buildComponent $ do
  intervalChan <- TChan.newTChanIO

  let
    triggerEvent :: IO ()
    triggerEvent = forever $ do
      threadDelay (delaySeconds * 1000100)
      atomically $ TChan.writeTChan intervalChan ()

    -- (7)
    intervalSpec :: Capataz.ProcessSpec
    intervalSpec =
      Capataz.workerSpec workerName triggerEvent
         (set Capataz.workerRestartStrategyL Capataz.Permanent)

  return (TChan.readTChan intervalChan, intervalSpec)
```

`(7)` We build a `ProcessSpec` using the `workerSpec` function, this worker will emit a signal after sleeping for a few seconds. Note, we use the `workerRestartStrategyL` lens to override the default options of a new worker.

```haskell
-- | Builds a "ProcessSpec" that monitors a green thread that receives messages
-- from the given "WatcherMsg" notifier, it receives a path where the git
-- repository is.
buildGitWorker
  :: FilePath -- ^ Location of git repository where changes are kept
  -> IO WatcherMsg -- ^ An IO sub-routine that gets triggered everytime a
                   -- WatcherMsg happens
  -> ProcessSpec
buildGitWorker !repoPath !getWatcherMsg =
  let
    executeCmd :: IO ()
    executeCmd = forever $ do
        msg <- getWatcherMsg
        case msg of
          FileChanged {} ->
            Shelly.shelly
              $ Shelly.chdir (Shelly.fromText $ Text.pack repoPath)
              $ do Shelly.run_ "git" ["add", "."]
                   Shelly.run_ "git" ["commit", "-a", "--amend", "--no-edit"]

          SyncRequested -> do
            Shelly.shelly
              $ Shelly.chdir (Shelly.fromText $ Text.pack repoPath)
              $ do Shelly.run_ "git" ["pull", "-r"]
                   Shelly.run_ "git" ["push", "--force-with-lease", "origin"]
  in
    -- (8)
    Capataz.workerSpec "git-worker" executeCmd
      (set Capataz.workerRestartStrategyL Capataz.Permanent)
```

`(8)` We build a `ProcessSpec` using the `workerSpec` function; this worker receives notifications and performs bash operations using the shelly API.

```haskell
-- | Returns both an utility function for logging and a "ProcessSpec" to
-- supervise a thread that receives log messages and displays them to stdout.
buildEventLogger :: ComponentM (DisplayBuilder -> IO (), ProcessSpec)
buildEventLogger = Component.buildComponent $ do
  logChan <- TChan.newTChanIO
  let
    logOptions :: Logger.LogOptions
    logOptions =
      Logger.LogOptions
      {
        Logger.logMinLevel = Logger.LevelDebug
      , Logger.logVerboseFormat = True
      , Logger.logTerminal = True
      , Logger.logUseTime = True
      , Logger.logUseColor = True
      , Logger.logUseUnicode = True
      }

    logLoop :: IO ()
    logLoop = Logger.withStickyLogger logOptions $ \logger -> do
      flip runReaderT logger $ forever $ do
        bs <- liftIO $ atomically $ TChan.readTChan logChan
        Logger.logDebug bs

  return (
      atomically . TChan.writeTChan logChan
      -- (9)
    , Capataz.workerSpec "logger" logLoop
        (set Capataz.workerRestartStrategyL Capataz.Permanent)
    )
```

`(9)` As with the previous examples, we create a `ProcessSpec` with the `workerSpec` function, this worker listens to a channel for things to log to stdout.

```haskell
-- | Creates a RepoWatcher supervisor, which is composed by:
--
-- * A file watcher
-- * An interval worker
-- * A git worker
--
buildRepoFileWatcher :: INotify.INotify -> FilePath -> ComponentM ProcessSpec
buildRepoFileWatcher !inotify !repoDir = do
  onFileChange <- buildFileWatcher inotify repoDir
  (onSync, syncIntervalSpec) <- buildIntervalWorker "git-sync-interval" (60 * 2)

  let
    onMsg :: IO WatcherMsg
    onMsg =
      atomically
      $ (FileChanged <$> onFileChange)
      `orElse` (onSync $> SyncRequested)

    gitWorkerSpec :: ProcessSpec
    gitWorkerSpec =
      buildGitWorker repoDir onMsg

  Component.buildComponent
    $ return
    -- (10)
    $ Capataz.supervisorSpec ("repo-file-watcher:" <> Text.pack repoDir)
        ( set Capataz.supervisorRestartStrategyL Capataz.OneForOne
        . set Capataz.supervisorProcessSpecListL
            [ gitWorkerSpec
            , syncIntervalSpec
            ]
        )

```

`(10)` With the `supervisorSpec` function, we create a supervisor `ProcessSpec`. Note we use the `supervisorProcessSpecListL` lens to add `ProcessSpec`s created on steps (7) and (8). This supervisor will start a gitWorker and a syncInterval processes on startup, and will keep track of the errors that they trigger.

```haskell
-- | Creates a Capataz supervision tree which contains a RepoWatcher
-- supervisor per repository path
createRepoWatcherSystem :: [FilePath] -> ComponentM Capataz
createRepoWatcherSystem repoPathList = do
  (logFn, loggerProcessSpec) <- buildEventLogger
  inotify <- buildINotify
  repoProcessSpecList <- mapM (buildRepoFileWatcher inotify) repoPathList

  let
    procList =
      loggerProcessSpec:repoProcessSpecList

  Component.buildComponentWithTeardown $ mask $ \_ -> do
    -- (11)
    capataz <- Capataz.forkCapataz
      ( set Capataz.onSystemEventL (logFn . displayShow)
      . set Capataz.supervisorProcessSpecListL procList
      )

    -- (12)
    return (capataz, Capataz.getCapatazTeardown capataz)
```

`(11)` We wire everything up using the `forkCapataz` function, we enhance the telemetry of our app by specifying a function that connects the events from Capataz to our logger monitored thread.

`(12)` We return our capataz record and a teardown sub-routine.

```haskell
main :: IO ()
main = do
  input <- getArgs
  case input of
    [] ->
      error "Expecting repository paths as inputs; got nothing"
    repoPaths ->
      -- (13)
      withComponent ("repo-watcher-system")
                    (createRepoWatcherSystem repoPaths)
                    Capataz.joinCapatazThread -- (14)

```

`(13)` We execute our `ComponentM` sub-routine, and provide a callback that receives the result from that sub-routine, guaranting also that all resources are cleaned up (even in the case of failure), when `(14)` ends.

`(14)` We use the `joinCapatazThread` which allows us to connect both our current thread and the root supervisor thread and lock.

## Try it out!

1) Clone the [capataz repository](https://github.com/roman/Haskell-capataz)

1) Run `make build`

2) Run `./out/bin/repo-watcher <repo-directory>`

3) Try modifying a file in the repo and seeing it automatically commit

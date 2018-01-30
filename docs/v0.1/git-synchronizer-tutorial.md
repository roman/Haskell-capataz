# Git repository synchronizer

We will build a CLI application that automatically synchronizes files in a git repository with its remote server whenever they are modified. To do this, we will keep track of file modifications using the iNotify UNIX API.

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

Our CLI program will receive various strings as CLI arguments; each argument represents a file path that points to a git repository which we want to synchronize with a remote repository automatically; by just saving a file a commit should be created; also, our repository will sync with its remote host in a regular basis.

You can find the code for this tutorial in the [`examples`](https://github.com/roman/Haskell-capataz/tree/master/examples/) directory of the project's Github repository.

## Setting up the stage - Modeling our domain problem as a supervisor tree

We first define a diagram of how our supervision tree is going to look like:

```text
capataz-system/
├── logger-worker  -- ^ performs logging to a console
└── repository-supervisor(*)
    ├── git-worker            -- ^ performs all git operations on a repository
    ├── repo-file-watcher     -- ^ monitors changes on files
    └── sync-interval-worker  -- ^ notifies every interval of time an event
```

Our Capataz' root supervisor tree is composed of a worker green thread responsible for logging, and at least one (1)  `repository-supervisor` that monitors two (2) worker threads, one responsible for sync interval (to schedule git pull/push commands) and one to execute git commands.

Why this organization? We want to make sure that each repository is self-contained, if one of them is faulty, we wouldn't want to restart the whole application, only the workers related to that repository. We can go as far as to add an `AllForOne` strategy at the `repository-supervisor` level.

## How do our workers communicate?

Given Capataz does not enforce a message passing communication scheme, we can use whatever we want to communicate our workers, from third-party services like SQS to external processes like RabbitMQ or Redis, to memory STM Queues and MVars.

Given we don't care about messages getting lost in case of catastrophic failure, in this example we are going to use `STM` channels (`TChan`) to communicate threads between them.

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
-- non-essential implementation detail.
instance Logger.HasLogFunc Logger.LogFunc where
  logFuncL = id

-- | Utility function that runs a component builder and uses it's result
-- on a callback on a safe way.
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
  -- | Message triggered when monitored file changes
  = FileChanged !FilePath
  -- | Message triggered when a sync request is made
  | SyncRequested
```

`(2)` We use a record that helps us notify threads when an interval period is met (`SyncRequest`) or when a monitored file has changed (triggered by `hinotify`).

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
  -> (FilePath -> IO ())
  -> FilePath -- ^ Directory where changes are tracked
  -> IO (IO ())
buildFileWatcher inotify notifyFileChange !dir = do
  -- (3)
  fileWatch <- INotify.addWatch inotify [INotify.CloseWrite, INotify.Modify] dir $ \ev -> do
    case ev of
      INotify.Modified {INotify.isDirectory, INotify.maybeFilePath}
        -- we ignore all changes that happen to a Directory
        | isDirectory -> return ()
        | otherwise ->
          maybe (return ()) notifyFileChange maybeFilePath
      _ -> return ()

  return (INotify.removeWatch fileWatch)
```

`(3)` Using the `INotify.addWatch` function, we can register what files we are interested in receiving events on modification; we also need to provide a callback that writes the `INotify` events to a `TChan` that we created in step `(4)`

```haskell
-- | Returns both an STM sub-routine that blocks until a given period
-- has passed, and a "ProcessSpec" for supervision of this interval thread.
buildIntervalWorker
  :: Text  -- ^ Name of the worker process
  -> Int   -- ^ Number of seconds between notifications
  -> ComponentM (STM (), ProcessSpec)
buildIntervalWorker !workerName !delaySeconds = Component.buildComponent $ do
  -- (4)
  intervalChan <- TChan.newTChanIO

  let
    triggerEvent :: IO ()
    triggerEvent = forever $ do
      threadDelay (delaySeconds * 1000100)
      atomically $ TChan.writeTChan intervalChan ()

    -- (5)
    intervalSpec :: Capataz.ProcessSpec
    intervalSpec =
      Capataz.workerSpec workerName triggerEvent
         (set Capataz.workerRestartStrategyL Capataz.Permanent)

  return (TChan.readTChan intervalChan, intervalSpec)
```

`(4)` We use a `TChan` to communicate to other interesting threads that a git synchronization should happen

`(5)` We build a `ProcessSpec` using the `workerSpec` function; this worker will emit a signal after sleeping for a few seconds. Note, we use the `workerRestartStrategyL` lens to override the default options of a new worker.

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
                   Shelly.run_ "git" ["commit", "-m", "file changes"]

          SyncRequested -> do
            Shelly.shelly
              $ Shelly.chdir (Shelly.fromText $ Text.pack repoPath)
              $ do Shelly.run_ "git" ["pull", "-r", "origin", "master"]
                   Shelly.run_ "git" ["push", "origin", "master"]
  in
    -- (6)
    Capataz.workerSpec "git-worker" executeCmd
      (set Capataz.workerRestartStrategyL Capataz.Permanent)
```

`(6)` We build a `ProcessSpec` using the `workerSpec` function; this worker receives notifications and performs bash operations using the `Shelly` API.

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
      -- (7)
    , Capataz.workerSpec "logger" logLoop
        (set Capataz.workerRestartStrategyL Capataz.Permanent)
    )
```

`(7)` As with the previous examples, we create a `ProcessSpec` with the `workerSpec` function; this worker listens to a channel for messages to print to `stdout`.

```haskell
-- | Creates a RepoWatcher supervisor, which is composed by:
--
-- * A file watcher
-- * An interval worker
-- * A git worker
--
-- NOTE: when we restart our repo file watcher, we need to make sure that our
-- watch gets restarted as well.
buildRepoFileWatcher :: INotify.INotify -> FilePath -> ComponentM ProcessSpec
buildRepoFileWatcher !inotify !repoDir = do
  -- We create functions that workers will use to communicate between each
  -- other
  changesChan <- liftIO $ TChan.newTChanIO


  let notifyFileChange = atomically . TChan.writeTChan changesChan
      onFileChange = TChan.readTChan changesChan

  fileWatchCleanupRef <- liftIO $ do
    fileWatchCleanup <- buildFileWatcher inotify notifyFileChange repoDir
    newIORef fileWatchCleanup

  (onSync, syncIntervalSpec) <- buildIntervalWorker "git-sync-interval" (60 * 2)

  let
    -- We compose both Sync interval requests and file changes notifications
    onMsg :: IO WatcherMsg
    onMsg =
      atomically
      $ (FileChanged <$> onFileChange)
      `orElse` (onSync $> SyncRequested)

    cleanupWatch :: IO ()
    cleanupWatch =
      -- Invokes the `IO ()` operation contained inside our `IORef`
      join (readIORef fileWatchCleanupRef)

    -- We restart the inotify watch when supervisor fails; We mask to make sure
    -- that our ref is not corrupted with async exceptions
    onRepoWatcherFailure :: IO ()
    onRepoWatcherFailure = mask $ \unmask -> do
      unmask cleanupWatch
      fileWatchCleanup <- buildFileWatcher inotify notifyFileChange repoDir
      writeIORef fileWatchCleanupRef fileWatchCleanup

    gitWorkerSpec :: ProcessSpec
    gitWorkerSpec =
      buildGitWorker repoDir onMsg

  -- (8)
  Component.buildComponentWithCleanup
    $ return
    $ (
        -- (9)
        Capataz.supervisorSpec ("repo-file-watcher:" <> Text.pack repoDir)
            ( set Capataz.supervisorRestartStrategyL Capataz.AllForOne -- (10)
            . set Capataz.supervisorOnFailureL (const $ onRepoWatcherFailure) -- (11)
            . set Capataz.supervisorProcessSpecListL [gitWorkerSpec, syncIntervalSpec]
            )
      , ("repo-file-watcher:" <> Text.pack repoDir, cleanupWatch)
      )
```

`(8)` The `Component.buildComponentWithCleanup` allows us to allocate resources, returning a value from this allocation (say, a supervisor spec) and a named `IO ()` sub-routine that gets composed with other cleanup tasks by the `ComponentM` monad.


`(9)` With the `supervisorSpec` function, we create a supervisor `ProcessSpec`. Note we use the `supervisorProcessSpecListL` lens to create a _static_ supervision tree; we use the `ProcessSpec` created on steps `(7)` and `(8)`. This supervisor will start a `git-worker` and a `sync-interval-worker` process on startup, and it will monitor and restart workers when they fail.

`(10)` We use an `AllForOne` strategy, meaning, if either `gitWorkerSpec` or `syncIntervalSpec` fail, the other is going to be restarted.

`(11)` We use the `supervisorOnFailureL` lens to override the repo supervisor failure callback, in here, we make sure we restart the `INotify` watch of the repository.

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
    -- (12)
    capataz <- Capataz.forkCapataz
      ( set Capataz.onSystemEventL (logFn . displayShow)
      . set Capataz.supervisorProcessSpecListL procList
      )

    -- (13)
    return (capataz, Capataz.getCapatazTeardown capataz)
```

`(12)` We connect all our components using the `forkCapataz` function. The logger notification function is used as our `onSystemEventL` callback so that we have proper logging around what our Capataz system is doing.

`(13)` We return our Capataz record and a [`teardown`](https://hackage.org/package/teardown) sub-routine.

```haskell
main :: IO ()
main = do
  input <- getArgs
  case input of
    [] ->
      error "Expecting repository paths as inputs; got nothing."
    repoPaths ->
      -- (14)
      withComponent ("repo-watcher-system")
                    (createRepoWatcherSystem repoPaths)
                    Capataz.joinCapatazThread -- (15)

```

`(14)` We execute our `ComponentM` sub-routine and provide a callback that receives the result from that sub-routine, also guaranteeing that all resources are cleaned up (even in the case of failure).

`(15)` We use the `joinCapatazThread` function, which allows us to connect both our current thread and the root supervisor thread and lock it until the Capataz root supervisor finishes its execution.

## What have we accomplished

By now we have a pretty reliable program that will stay running smoothly even in errors scenarios like:

* We monitor a directory that is _not_ a git repository
* The repository is not configured to have a remote host
* The repository gets deleted

## Try it out!

1) Clone the [capataz repository](https://github.com/roman/Haskell-capataz)

2) Run `make build`

3) Run `./out/bin/repo-watcher <some-git-repo-directory>`

4) Try modifying a file in the monitored repo and see how it perform a commit automatically.

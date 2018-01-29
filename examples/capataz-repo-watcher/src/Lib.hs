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
import RIO
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

import qualified Shelly
import qualified System.INotify as INotify

--------------------------------------------------------------------------------

-- | An instance of HasLogFunc for a LogFunc, did this given it was not provided
-- by RIO, there must be a better way to do this.
instance Logger.HasLogFunc Logger.LogFunc where
  logFuncL = id

-- | A combination of both file modification events and git repository
-- synchronization.
data WatcherMsg
  -- | Message triggered when a monitored file changes
  = FileChanged !FilePath
  -- | Message triggered when a sync request is made
  | SyncRequested

--------------------------------------------------------------------------------

-- | Utility function that should be included in the teardown library, no need
-- to understand what is this used for.
withComponent :: Text -> ComponentM a -> (a -> IO ()) -> IO ()
withComponent !appDesc !buildComponent !f = mask $ \restore -> do
  component <- Component.runComponentM appDesc buildComponent
  (restore . f $ Component.fromComponent component)
      `finally`
      (Component.teardown component >>= Prelude.print)

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
buildFileWatcher inotify !dir = Component.buildComponentWithCleanup $ mask $ \_ -> do
  fileChangesChan <- TChan.newTChanIO
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

  return ( TChan.readTChan fileChangesChan
         , ("inotify:" <> Text.pack dir, INotify.removeWatch fileWatch)
         )

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

    -- (3)
    intervalSpec :: Capataz.ProcessSpec
    intervalSpec =
      Capataz.workerSpec workerName triggerEvent
         (set Capataz.workerRestartStrategyL Capataz.Permanent)

  return (TChan.readTChan intervalChan, intervalSpec)

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
    -- (4)
    Capataz.workerSpec "git-worker" executeCmd
      (set Capataz.workerRestartStrategyL Capataz.Permanent)

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
      -- (5)
    , Capataz.workerSpec "logger" logLoop
        (set Capataz.workerRestartStrategyL Capataz.Permanent)
    )

-- | Creates a RepoWatcher supervisor, which is composed by:
--
-- * A file watcher
-- * An interval worker
-- * A git worker
--
buildRepoFileWatcher :: INotify.INotify -> FilePath -> ComponentM ProcessSpec
buildRepoFileWatcher !inotify !repoDir = do
  -- (6)
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

  -- (7)
  Component.buildComponent
    $ return
    $ Capataz.supervisorSpec ("repo-file-watcher:" <> Text.pack repoDir)
        ( set Capataz.supervisorRestartStrategyL Capataz.OneForOne
        . set Capataz.supervisorProcessSpecListL
            [ gitWorkerSpec
            , syncIntervalSpec
            ]
        )

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
    -- (8)
    capataz <- Capataz.forkCapataz
      ( set Capataz.onSystemEventL (logFn . displayShow)
      . set Capataz.supervisorProcessSpecListL procList
      )

    -- (9)
    return (capataz, Capataz.getCapatazTeardown capataz)


main :: IO ()
main = do
  input <- getArgs
  case input of
    [] ->
      error "Expecting repository paths as inputs; got nothing"
    repoPaths ->
      withComponent ("repo-watcher-system")
                    (createRepoWatcherSystem repoPaths)
                    Capataz.joinCapatazThread -- (10)

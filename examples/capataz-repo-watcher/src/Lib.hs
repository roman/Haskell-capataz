{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib where

import qualified Prelude
import           RIO
import qualified RIO.Logger as Logger

import Control.Monad.Component (ComponentM)

import Control.Concurrent         (threadDelay)
import Control.Concurrent.Capataz (Capataz, ProcessSpec)
import Control.Concurrent.STM     (orElse)

import System.Environment (getArgs)

import qualified RIO.Text as Text

import qualified Control.Concurrent.Capataz   as Capataz
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Monad.Component      as Component

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
    `finally` (Component.teardown component >>= Prelude.print)

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
  fileWatch <-
    INotify.addWatch inotify [INotify.CloseWrite, INotify.Modify] dir $ \ev ->

      case ev of
        INotify.Modified { INotify.isDirectory, INotify.maybeFilePath }
          |
          -- we ignore all changes that happen to a Directory
            isDirectory -> return ()
          | otherwise   -> maybe (return ()) notifyFileChange maybeFilePath
        _ -> return ()

  return (INotify.removeWatch fileWatch)

-- | Returns both an STM sub-routine that blocks until a given period of time
-- has passed, and a "ProcessSpec" for supervision of this interval thread.
buildIntervalWorker
  :: Text  -- ^ Name of the worker process
  -> Int   -- ^ Number of seconds between notifications
  -> ComponentM (STM (), ProcessSpec)
buildIntervalWorker !workerName !delaySeconds = Component.buildComponent $ do
  intervalChan <- TChan.newTChanIO

  let triggerEvent :: IO ()
      triggerEvent = forever $ do
        threadDelay (delaySeconds * 1000100)
        atomically $ TChan.writeTChan intervalChan ()

      intervalSpec :: Capataz.ProcessSpec
      intervalSpec = Capataz.workerSpec
        workerName
        triggerEvent
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
        FileChanged{} ->
          Shelly.shelly
            $ Shelly.chdir (Shelly.fromText $ Text.pack repoPath)
            $ do
                Shelly.run_ "git" ["add", "."]
                Shelly.run_ "git" ["commit", "-m", "file changes"]

        SyncRequested ->
          Shelly.shelly
            $ Shelly.chdir (Shelly.fromText $ Text.pack repoPath)
            $ do
                Shelly.run_ "git" ["pull", "-r", "origin", "master"]
                Shelly.run_ "git" ["push", "origin", "master"]
  in
    Capataz.workerSpec "git-worker"
                       executeCmd
                       (set Capataz.workerRestartStrategyL Capataz.Permanent)

-- | Returns both an utility function for logging and a "ProcessSpec" to
-- supervise a thread that receives log messages and displays them to stdout.
buildEventLogger :: ComponentM (DisplayBuilder -> IO (), ProcessSpec)
buildEventLogger = Component.buildComponent $ do
  logChan <- TChan.newTChanIO
  let logOptions :: Logger.LogOptions
      logOptions = Logger.LogOptions
        { Logger.logMinLevel      = Logger.LevelDebug
        , Logger.logVerboseFormat = True
        , Logger.logTerminal      = True
        , Logger.logUseTime       = True
        , Logger.logUseColor      = True
        , Logger.logUseUnicode    = True
        }

      logLoop :: IO ()
      logLoop = Logger.withStickyLogger logOptions $ \logger ->
        flip runReaderT logger $ forever $ do
          bs <- liftIO $ atomically $ TChan.readTChan logChan
          Logger.logDebug bs

  return
    ( atomically . TChan.writeTChan logChan
    , Capataz.workerSpec
      "logger"
      logLoop
      (set Capataz.workerRestartStrategyL Capataz.Permanent)
    )

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
  changesChan <- liftIO TChan.newTChanIO
  let notifyFileChange = atomically . TChan.writeTChan changesChan
      onFileChange     = TChan.readTChan changesChan

  fileWatchCleanupRef <- liftIO $ do
    fileWatchCleanup <- buildFileWatcher inotify notifyFileChange repoDir
    newIORef fileWatchCleanup

  (onSync, syncIntervalSpec) <- buildIntervalWorker "git-sync-interval" (60 * 2)

  let
    -- We compose both Sync interval requests and file changes notifications
    onMsg :: IO WatcherMsg
    onMsg =
      atomically
        $        (FileChanged <$> onFileChange)
        `orElse` (onSync $> SyncRequested)

    cleanupWatch :: IO ()
    cleanupWatch = join (readIORef fileWatchCleanupRef)

    -- We restart the inotify watch when supervisor fails; We mask to make sure
    -- that our ref is not corrupted with async exceptions
    onRepoWatcherFailure :: IO ()
    onRepoWatcherFailure = mask $ \unmask -> do
      unmask cleanupWatch
      fileWatchCleanup <- buildFileWatcher inotify notifyFileChange repoDir
      writeIORef fileWatchCleanupRef fileWatchCleanup

    gitWorkerSpec :: ProcessSpec
    gitWorkerSpec = buildGitWorker repoDir onMsg

  Component.buildComponentWithCleanup $ return
    ( Capataz.supervisorSpec
      ("repo-file-watcher:" <> Text.pack repoDir)
      ( set Capataz.supervisorRestartStrategyL Capataz.AllForOne
      . set Capataz.supervisorOnFailureL       (const $ onRepoWatcherFailure)
      . set Capataz.supervisorProcessSpecListL
            [gitWorkerSpec, syncIntervalSpec]
      )
    , ("repo-file-watcher:" <> Text.pack repoDir, cleanupWatch)
    )

-- | Creates a Capataz supervision tree which contains a RepoWatcher
-- supervisor per repository path
createRepoWatcherSystem :: [FilePath] -> ComponentM Capataz
createRepoWatcherSystem repoPathList = do
  (logFn, loggerProcessSpec) <- buildEventLogger
  inotify                    <- buildINotify
  repoProcessSpecList        <- mapM (buildRepoFileWatcher inotify) repoPathList

  let procList = loggerProcessSpec : repoProcessSpecList

  Component.buildComponentWithTeardown $ do
    capataz <- Capataz.forkCapataz
      ( set Capataz.onSystemEventL             (logFn . displayShow)
      . set Capataz.supervisorProcessSpecListL procList
      )

    return (capataz, Capataz.getCapatazTeardown capataz)


main :: IO ()
main = do
  input <- getArgs
  case input of
    []        -> error "Expecting repository paths as inputs; got nothing"
    repoPaths -> withComponent "repo-watcher"
                               (createRepoWatcherSystem repoPaths)
                               Capataz.joinCapatazThread

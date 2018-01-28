{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}

{-| This module contains all logic related to error handling when spawning threads
  to execute Worker sub-routines
-}
module Control.Concurrent.Capataz.Internal.Worker where

import Protolude

import Control.Concurrent.Async (asyncWithUnmask)
import Data.Time.Clock          (getCurrentTime)

import qualified Data.UUID.V4 as UUID

import qualified Control.Concurrent.Capataz.Internal.Process as Process
import qualified Control.Concurrent.Capataz.Internal.Util    as Util

import Control.Concurrent.Capataz.Internal.Types

-- | Decorates the given @IO ()@ sub-routine with failure handling
workerMain
  :: ParentSupervisorEnv -> WorkerOptions -> WorkerId -> RestartCount -> IO Worker
workerMain env@ParentSupervisorEnv { supervisorNotify } workerOptions@WorkerOptions { workerName, workerAction } workerId restartCount
  = do
    workerCreationTime <- getCurrentTime
    workerAsync        <- asyncWithUnmask $ \unmask -> do

      eResult <- try $ do
        Util.setProcessThreadName workerId workerName
        unmask workerAction

      resultEvent <- case eResult of
        Left err -> Process.handleProcessException unmask
                                                   env
                                                   (WorkerSpec workerOptions)
                                                   workerId
                                                   restartCount
                                                   err
        Right _ -> Process.handleProcessCompletion unmask
                                                   env
                                                   (WorkerSpec workerOptions)
                                                   workerId
                                                   restartCount

      supervisorNotify (MonitorEvent resultEvent)

    return Worker
      { workerId
      , workerName
      , workerAsync
      , workerCreationTime
      , workerOptions
      }

-- | Internal function that forks a worker thread on the Capataz thread; note
-- this is different from the public @forkWorker@ function which sends a message
-- to the capataz loop
forkWorker
  :: ParentSupervisorEnv
  -> WorkerOptions
  -> Maybe (WorkerId, RestartCount)
  -> IO Worker
forkWorker env workerOptions mRestartInfo = do
  (workerId, restartCount) <- case mRestartInfo of
    Just (workerId, restartCount) -> pure (workerId, restartCount)
    Nothing                       -> (,) <$> UUID.nextRandom <*> pure 0

  worker <- workerMain env workerOptions workerId restartCount
  Process.notifyProcessStarted mRestartInfo env (WorkerProcess worker)
  return worker

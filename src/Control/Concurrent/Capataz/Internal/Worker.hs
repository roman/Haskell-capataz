{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
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
  :: SupervisorEnv -> WorkerSpec -> WorkerId -> RestartCount -> IO Worker
workerMain env@SupervisorEnv { supervisorNotify } workerSpec@WorkerSpec { workerName, workerAction } workerId restartCount
  = do
    let parentEnv = Util.toParentSupervisorEnv env
    workerCreationTime <- getCurrentTime
    workerAsync        <- asyncWithUnmask $ \unmask -> do

      eResult <- try $ do
        Util.setProcessThreadName workerId workerName
        unmask workerAction

      resultEvent <- case eResult of
        Left err -> Process.handleProcessException
          unmask
          parentEnv
          (WorkerProcessSpec workerSpec)
          workerId
          restartCount
          err
        Right _ -> Process.handleProcessCompletion
          unmask
          parentEnv
          (WorkerProcessSpec workerSpec)
          workerId
          restartCount

      supervisorNotify (MonitorEvent resultEvent)

    return Worker
      { workerId
      , workerName
      , workerAsync
      , workerCreationTime
      , workerSpec
      }

-- | Internal function used to send a proper "CapatazEvent" to the "notifyEvent"
-- callback, this event can either be a @WorkerStarted@ or a @WorkerRestarted@
notifyWorkerStarted :: Maybe (WorkerId, Int) -> SupervisorEnv -> Worker -> IO ()
notifyWorkerStarted mRestartInfo SupervisorEnv { supervisorId, supervisorName, notifyEvent } Worker { workerId, workerName, workerAsync }
  = do
    let processId   = workerId
        processName = workerName
    eventTime <- getCurrentTime
    case mRestartInfo of
      Just (_workerId, processRestartCount) -> notifyEvent ProcessRestarted
        { supervisorId
        , supervisorName
        , processId
        , processName
        , processRestartCount
        , processThreadId     = asyncThreadId workerAsync
        , eventTime
        }
      Nothing -> notifyEvent ProcessStarted
        { supervisorId
        , supervisorName
        , processId
        , processName
        , eventTime
        , processThreadId = asyncThreadId workerAsync
        }

-- | Internal function that forks a worker thread on the Capataz thread; note
-- this is different from the public @forkWorker@ function which sends a message
-- to the capataz loop
forkWorker
  :: SupervisorEnv -> WorkerSpec -> Maybe (WorkerId, RestartCount) -> IO Worker
forkWorker env workerSpec mRestartInfo = do
  (workerId, restartCount) <- case mRestartInfo of
    Just (workerId, restartCount) -> pure (workerId, restartCount)
    Nothing                       -> (,) <$> UUID.nextRandom <*> pure 0

  worker <- workerMain env workerSpec workerId restartCount
  notifyWorkerStarted mRestartInfo env worker
  return worker

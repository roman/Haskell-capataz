{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-| This module contains all logic related to the restart of workers -}
module Control.Concurrent.Capataz.Internal.Restart where

import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

import Protolude

import qualified Data.HashMap.Strict as HashMap

import           Control.Concurrent.Capataz.Internal.Types
import qualified Control.Concurrent.Capataz.Internal.Util   as Util
import qualified Control.Concurrent.Capataz.Internal.Worker as Worker

--------------------------------------------------------------------------------

-- | Function used to track difference between two timestamps to track capataz
-- error intensity
calcDiffSeconds :: UTCTime -> IO NominalDiffTime
calcDiffSeconds creationTime = do
  currentTime <- getCurrentTime
  return $ diffUTCTime currentTime creationTime

-- | Function that checks restart counts and worker start time to assess if the
-- capataz error intensity has been breached, see "WorkerRestartAction" for
-- possible outcomes.
calcRestartAction
  :: SupervisorEnv -> Int -> NominalDiffTime -> WorkerRestartAction
calcRestartAction SupervisorEnv { supervisorIntensity, supervisorPeriodSeconds } restartCount diffSeconds
  = case () of
    _
      | diffSeconds
        <  supervisorPeriodSeconds
        && restartCount
        >  supervisorIntensity
      -> HaltCapataz
      | diffSeconds > supervisorPeriodSeconds
      -> ResetRestartCount
      | otherwise
      -> IncreaseRestartCount

-- | Sub-routine responsible of executing a "SupervisorRestartStrategy"
execCapatazRestartStrategy :: SupervisorEnv -> WorkerEnv -> Int -> IO ()
execCapatazRestartStrategy supervisorEnv@SupervisorEnv { supervisorRestartStrategy } WorkerEnv { workerId, workerSpec } workerRestartCount
  = case supervisorRestartStrategy of
    AllForOne -> do
      newProcessList <- restartWorkers supervisorEnv workerId workerRestartCount
      let newProcessMap =
            newProcessList
              & fmap (\process -> (Util.getProcessId process, process))
              & HashMap.fromList
      Util.resetProcessMap supervisorEnv (const newProcessMap)

    OneForOne -> do
      Util.removeWorkerFromMap supervisorEnv workerId
      newWorker <- restartWorker supervisorEnv
                                 workerSpec
                                 workerId
                                 workerRestartCount
      Util.appendProcessToMap supervisorEnv (WorkerProcess newWorker)

-- | Executes a restart action returned from the invokation of "calcRestartAction"
execRestartAction :: SupervisorEnv -> WorkerEnv -> Int -> IO ()
execRestartAction supervisorEnv@SupervisorEnv { supervisorOnIntensityReached } workerEnv@WorkerEnv { workerId, workerName, workerCreationTime } workerRestartCount
  = do
    restartAction <- calcRestartAction supervisorEnv workerRestartCount
      <$> calcDiffSeconds workerCreationTime

    case restartAction of
      HaltCapataz -> do
        -- skip exceptions on callback
        (_ :: Either SomeException ()) <- try supervisorOnIntensityReached
        throwIO CapatazIntensityReached
          { processId           = workerId
          , processName         = workerName
          , processRestartCount = succ workerRestartCount
          }

      ResetRestartCount -> execCapatazRestartStrategy supervisorEnv workerEnv 0

      IncreaseRestartCount -> execCapatazRestartStrategy
        supervisorEnv
        workerEnv
        (succ workerRestartCount)

--------------------------------------------------------------------------------

-- | Restarts _all_ the worker green thread of a Capataz, invoked when one
-- worker green thread fails and causes sibling worker threads to get restarted
-- as well
restartWorkers :: SupervisorEnv -> WorkerId -> RestartCount -> IO [Process]
restartWorkers supervisorEnv@SupervisorEnv { supervisorProcessTerminationOrder } failingWorkerId restartCount
  = do
    processMap <- Util.readProcessMap supervisorEnv

    let processList = Util.sortProcessesByTerminationOrder
          supervisorProcessTerminationOrder
          processMap

    newProcessList <- forM processList $ \process -> case process of
      WorkerProcess worker@Worker { workerId, workerSpec } -> do
        unless (failingWorkerId == workerId)
          $ forceRestartWorker supervisorEnv worker

        let WorkerSpec { workerRestartStrategy } = workerSpec
        case workerRestartStrategy of
          Temporary -> return Nothing
          _ ->
            (Just . WorkerProcess)
              <$> restartWorker supervisorEnv workerSpec workerId restartCount
      _ -> panic "pending"

    return $ catMaybes newProcessList

-- | Sub-routine that is used when there is a restart request to a Worker caused
-- by an "AllForOne" restart from a failing sibling worker.
forceRestartWorker :: SupervisorEnv -> Worker -> IO ()
forceRestartWorker SupervisorEnv { supervisorId, supervisorName, notifyEvent } Worker { workerId, workerName, workerAsync }
  = do
    eventTime <- getCurrentTime
    notifyEvent ProcessTerminated
      { supervisorId
      , supervisorName
      , processThreadId   = asyncThreadId workerAsync
      , processId         = workerId
      , processName       = workerName
      , processType       = WorkerType
      , eventTime
      , terminationReason = "forced restart"
      }
    cancelWith workerAsync RestartProcessException

-- | Starts a new worker thread taking into account an existing "WorkerId" and
-- keeping a "RestartCount" to manage Capataz error intensity.
restartWorker
  :: SupervisorEnv -> WorkerSpec -> WorkerId -> RestartCount -> IO Worker
restartWorker supervisorEnv workerSpec workerId restartCount =
  Worker.forkWorker supervisorEnv workerSpec (Just (workerId, restartCount))

--------------------------------------------------------------------------------

-- | This sub-routine is responsible of the restart strategies execution when a
-- supervised worker finishes it execution because of a completion (e.g. worker
-- sub-routine finished without any errors).
handleWorkerCompleted :: SupervisorEnv -> WorkerId -> UTCTime -> IO ()
handleWorkerCompleted env@SupervisorEnv { supervisorId, supervisorName, notifyEvent } workerId eventTime
  = do
    mWorkerEnv <- Util.fetchWorkerEnv env workerId
    case mWorkerEnv of
      Nothing -> return ()
      Just workerEnv@WorkerEnv { workerName, workerAsync, workerRestartStrategy }
        -> do
          notifyEvent ProcessCompleted
            { supervisorId
            , supervisorName
            , processId       = workerId
            , processName     = workerName
            , processType     = WorkerType
            , processThreadId = asyncThreadId workerAsync
            , eventTime
            }
          case workerRestartStrategy of
            Permanent -> do
              -- NOTE: Completed workers should never account as errors happening on
              -- a supervised thread, ergo, they should be restarted every time.

              -- TODO: Notify a warning around having a workerRestartStrategy different
              -- than Temporary on workers that may complete.
              let restartCount = 0
              execRestartAction env workerEnv restartCount

            _ -> Util.removeWorkerFromMap env workerId

-- | This sub-routine is responsible of the restart strategies execution when a
-- supervised worker finishes it execution because of a failure.
handleWorkerFailed :: SupervisorEnv -> WorkerId -> SomeException -> Int -> IO ()
handleWorkerFailed env@SupervisorEnv { supervisorId, supervisorName, notifyEvent } workerId workerError restartCount
  = do
    mWorkerEnv <- Util.fetchWorkerEnv env workerId
    case mWorkerEnv of
      Nothing -> return ()
      Just workerEnv@WorkerEnv { workerName, workerAsync, workerRestartStrategy }
        -> do
          eventTime <- getCurrentTime
          notifyEvent ProcessFailed
            { supervisorId
            , supervisorName
            , processId       = workerId
            , processName     = workerName
            , processType     = WorkerType
            , processError    = workerError
            , processThreadId = asyncThreadId workerAsync
            , eventTime
            }
          case workerRestartStrategy of
            Temporary -> Util.removeWorkerFromMap env workerId
            _         -> execRestartAction env workerEnv restartCount

-- | This sub-routine is responsible of the restart strategies execution when a
-- supervised worker finishes it execution because of a termination.
handleWorkerTerminated :: SupervisorEnv -> WorkerId -> Text -> Int -> IO ()
handleWorkerTerminated env@SupervisorEnv { supervisorId, supervisorName, notifyEvent } workerId terminationReason workerRestartCount
  = do
    mWorkerEnv <- Util.fetchWorkerEnv env workerId
    case mWorkerEnv of
      Nothing -> return ()
      Just workerEnv@WorkerEnv { workerName, workerAsync, workerRestartStrategy }
        -> do
          eventTime <- getCurrentTime
          notifyEvent ProcessTerminated
            { supervisorId
            , supervisorName
            , processId         = workerId
            , processName       = workerName
            , processType       = WorkerType
            , terminationReason
            , eventTime
            , processThreadId   = asyncThreadId workerAsync
            }
          case workerRestartStrategy of
            Permanent -> execRestartAction env workerEnv workerRestartCount
            _         -> Util.removeWorkerFromMap env workerId

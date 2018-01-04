{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-| This module contains all logic related to the restart of workers -}
module Control.Concurrent.Internal.Capataz.Restart where

import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

import Protolude

import           Control.Concurrent.Internal.Capataz.Types
import           Control.Concurrent.Internal.Capataz.Util
    ( appendProcessToMap
    , fetchWorkerEnv
    , getProcessId
    , readProcessMap
    , removeWorkerFromMap
    , resetProcessMap
    , sortProcessesByTerminationOrder
    )
import qualified Control.Concurrent.Internal.Capataz.Worker as Worker
import qualified Data.HashMap.Strict                        as HashMap

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
calcRestartAction :: CapatazEnv -> Int -> NominalDiffTime -> WorkerRestartAction
calcRestartAction CapatazEnv { capatazIntensity, capatazPeriodSeconds } restartCount diffSeconds
  = case () of
    _
      | diffSeconds < capatazPeriodSeconds && restartCount > capatazIntensity
      -> HaltCapataz
      | diffSeconds > capatazPeriodSeconds
      -> ResetRestartCount
      | otherwise
      -> IncreaseRestartCount

-- | Sub-routine responsible of executing a "CapatazRestartStrategy"
execCapatazRestartStrategy :: CapatazEnv -> WorkerEnv -> Int -> IO ()
execCapatazRestartStrategy capatazEnv@CapatazEnv { capatazRestartStrategy } WorkerEnv { workerId, workerSpec } workerRestartCount
  = case capatazRestartStrategy of
    AllForOne -> do
      newProcessList <- restartWorkers capatazEnv workerId workerRestartCount
      let newProcessMap =
            newProcessList
              & fmap (\process -> (getProcessId process, process))
              & HashMap.fromList
      resetProcessMap capatazEnv (const newProcessMap)

    OneForOne -> do
      removeWorkerFromMap capatazEnv workerId
      newWorker <- restartWorker capatazEnv
                                 workerSpec
                                 workerId
                                 workerRestartCount
      appendProcessToMap capatazEnv (WorkerProcess newWorker)

-- | Executes a restart action returned from the invokation of "calcRestartAction"
execRestartAction :: CapatazEnv -> WorkerEnv -> Int -> IO ()
execRestartAction capatazEnv@CapatazEnv { onCapatazIntensityReached } workerEnv@WorkerEnv { workerId, workerName, workerCreationTime } workerRestartCount
  = do
    restartAction <- calcRestartAction capatazEnv workerRestartCount
      <$> calcDiffSeconds workerCreationTime

    case restartAction of
      HaltCapataz -> do
        -- skip exceptions on callback
        (_ :: Either SomeException ()) <- try onCapatazIntensityReached
        throwIO CapatazIntensityReached
          { workerId
          , workerName
          , workerRestartCount = succ workerRestartCount
          }

      ResetRestartCount    -> execCapatazRestartStrategy capatazEnv workerEnv 0

      IncreaseRestartCount -> execCapatazRestartStrategy
        capatazEnv
        workerEnv
        (succ workerRestartCount)

--------------------------------------------------------------------------------

-- | Restarts _all_ the worker green thread of a Capataz, invoked when one
-- worker green thread fails and causes sibling worker threads to get restarted
-- as well
restartWorkers :: CapatazEnv -> WorkerId -> RestartCount -> IO [Process]
restartWorkers capatazEnv@CapatazEnv { capatazProcessTerminationOrder } failingWorkerId restartCount
  = do
    processMap <- readProcessMap capatazEnv

    let processList = sortProcessesByTerminationOrder
          capatazProcessTerminationOrder
          processMap

    newProcessList <- forM processList $ \process -> case process of
      WorkerProcess worker@Worker { workerId, workerSpec } -> do
        unless (failingWorkerId == workerId)
          $ forceRestartWorker capatazEnv worker

        let WorkerSpec { workerRestartStrategy } = workerSpec
        case workerRestartStrategy of
          Temporary -> return Nothing
          _ ->
            (Just . WorkerProcess)
              <$> restartWorker capatazEnv workerSpec workerId restartCount
      _ -> panic "pending"

    return $ catMaybes newProcessList

-- | Sub-routine that is used when there is a restart request to a Worker caused
-- by an "AllForOne" restart from a failing sibling worker.
forceRestartWorker :: CapatazEnv -> Worker -> IO ()
forceRestartWorker CapatazEnv { capatazName, capatazId, notifyEvent } Worker { workerId, workerName, workerAsync }
  = do
    eventTime <- getCurrentTime
    notifyEvent WorkerTerminated
      { capatazName
      , capatazId
      , workerId
      , workerName
      , eventTime
      , workerThreadId    = asyncThreadId workerAsync
      , terminationReason = "forced restart"
      }
    cancelWith workerAsync RestartWorkerException

-- | Starts a new worker thread taking into account an existing "WorkerId" and
-- keeping a "RestartCount" to manage Capataz error intensity.
restartWorker
  :: CapatazEnv -> WorkerSpec -> WorkerId -> RestartCount -> IO Worker
restartWorker capatazEnv workerSpec workerId restartCount =
  Worker.forkWorker capatazEnv workerSpec (Just (workerId, restartCount))

--------------------------------------------------------------------------------

-- | This sub-routine is responsible of the restart strategies execution when a
-- supervised worker finishes it execution because of a completion (e.g. worker
-- sub-routine finished without any errors).
handleWorkerCompleted :: CapatazEnv -> WorkerId -> UTCTime -> IO ()
handleWorkerCompleted env@CapatazEnv { capatazName, capatazId, notifyEvent } workerId eventTime
  = do
    mWorkerEnv <- fetchWorkerEnv env workerId
    case mWorkerEnv of
      Nothing -> return ()
      Just workerEnv@WorkerEnv { workerName, workerAsync, workerRestartStrategy }
        -> do
          notifyEvent WorkerCompleted
            { capatazId
            , capatazName
            , workerId
            , workerName
            , eventTime
            , workerThreadId = asyncThreadId workerAsync
            }
          case workerRestartStrategy of
            Permanent -> do
              -- NOTE: Completed workers should never account as errors happening on
              -- a supervised thread, ergo, they should be restarted every time.

              -- TODO: Notify a warning around having a workerRestartStrategy different
              -- than Temporary on workers that may complete.
              let restartCount = 0
              execRestartAction env workerEnv restartCount

            _ -> removeWorkerFromMap env workerId

-- | This sub-routine is responsible of the restart strategies execution when a
-- supervised worker finishes it execution because of a failure.
handleWorkerFailed :: CapatazEnv -> WorkerId -> SomeException -> Int -> IO ()
handleWorkerFailed env@CapatazEnv { capatazName, capatazId, notifyEvent } workerId workerError restartCount
  = do
    mWorkerEnv <- fetchWorkerEnv env workerId
    case mWorkerEnv of
      Nothing -> return ()
      Just workerEnv@WorkerEnv { workerName, workerAsync, workerRestartStrategy }
        -> do
          eventTime <- getCurrentTime
          notifyEvent WorkerFailed
            { capatazName
            , capatazId
            , workerId
            , workerName
            , workerError
            , workerThreadId = asyncThreadId workerAsync
            , eventTime
            }
          case workerRestartStrategy of
            Temporary -> removeWorkerFromMap env workerId
            _         -> execRestartAction env workerEnv restartCount

-- | This sub-routine is responsible of the restart strategies execution when a
-- supervised worker finishes it execution because of a termination.
handleWorkerTerminated :: CapatazEnv -> WorkerId -> Text -> Int -> IO ()
handleWorkerTerminated env@CapatazEnv { capatazName, capatazId, notifyEvent } workerId terminationReason workerRestartCount
  = do
    mWorkerEnv <- fetchWorkerEnv env workerId
    case mWorkerEnv of
      Nothing -> return ()
      Just workerEnv@WorkerEnv { workerName, workerAsync, workerRestartStrategy }
        -> do
          eventTime <- getCurrentTime
          notifyEvent WorkerTerminated
            { capatazName
            , capatazId
            , workerId
            , workerName
            , eventTime
            , terminationReason
            , workerThreadId    = asyncThreadId workerAsync
            }
          case workerRestartStrategy of
            Permanent -> execRestartAction env workerEnv workerRestartCount
            _         -> removeWorkerFromMap env workerId

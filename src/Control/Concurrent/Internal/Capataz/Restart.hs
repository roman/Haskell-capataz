{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Control.Concurrent.Internal.Capataz.Restart where

import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

import Protolude

import qualified Control.Concurrent.Internal.Capataz.Worker as Worker
import           Control.Concurrent.Internal.Capataz.Types
import           Control.Concurrent.Internal.Capataz.Util
    ( appendWorkerToMap
    , fetchWorkerEnv
    , readWorkerMap
    , removeWorkerFromMap
    , resetWorkerMap
    , sortWorkersByTerminationOrder
    )
import qualified Data.HashMap.Strict                          as HashMap

--------------------------------------------------------------------------------

calcDiffSeconds :: UTCTime -> IO NominalDiffTime
calcDiffSeconds creationTime = do
  currentTime <- getCurrentTime
  return $ diffUTCTime currentTime creationTime

calcRestartAction
  :: CapatazEnv -> Int -> NominalDiffTime -> WorkerRestartAction
calcRestartAction CapatazEnv { capatazIntensity, capatazPeriodSeconds } restartCount diffSeconds
  = case () of
    _
      | diffSeconds
        <  capatazPeriodSeconds
        && restartCount
        >  capatazIntensity
      -> HaltCapataz
      | diffSeconds > capatazPeriodSeconds
      -> ResetRestartCount
      | otherwise
      -> IncreaseRestartCount

execCapatazRestartStrategy :: CapatazEnv -> WorkerEnv -> Int -> IO ()
execCapatazRestartStrategy capatazEnv@CapatazEnv { capatazRestartStrategy } WorkerEnv { workerId, workerSpec } workerRestartCount
  = case capatazRestartStrategy of
    AllForOne -> do
      newWorkers <- restartWorkers capatazEnv workerId workerRestartCount
      let newWorkersMap =
            newWorkers
              & fmap (\worker@Worker { workerId = cid } -> (cid, worker))
              & HashMap.fromList
      resetWorkerMap capatazEnv (const newWorkersMap)

    OneForOne -> do
      removeWorkerFromMap capatazEnv workerId
      newWorker <- restartWorker capatazEnv workerSpec workerId workerRestartCount
      appendWorkerToMap capatazEnv newWorker

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

      ResetRestartCount ->
        execCapatazRestartStrategy capatazEnv workerEnv 0

      IncreaseRestartCount -> execCapatazRestartStrategy
        capatazEnv
        workerEnv
        (succ workerRestartCount)

--------------------------------------------------------------------------------

restartWorkers :: CapatazEnv -> WorkerId -> RestartCount -> IO [Worker]
restartWorkers capatazEnv@CapatazEnv { capatazWorkerTerminationOrder } failingWorkerId restartCount
  = do
    workerMap <- readWorkerMap capatazEnv

    let workers = sortWorkersByTerminationOrder
          capatazWorkerTerminationOrder
          workerMap

    newWorkers <- forM workers $ \worker@Worker { workerId, workerSpec } -> do
      unless (failingWorkerId == workerId) $ forceRestartWorker capatazEnv worker

      let WorkerSpec { workerRestartStrategy } = workerSpec
      case workerRestartStrategy of
        Temporary -> return Nothing
        _         -> Just <$> restartWorker capatazEnv workerSpec workerId restartCount

    return $ catMaybes newWorkers

forceRestartWorker :: CapatazEnv -> Worker -> IO ()
forceRestartWorker CapatazEnv { capatazName, capatazId, notifyEvent } Worker { workerId, workerName, workerAsync }
  = do
    eventTime <- getCurrentTime
    notifyEvent SupervisedWorkerTerminated
      { capatazName
      , capatazId
      , workerId
      , workerName
      , eventTime
      , workerThreadId     = asyncThreadId workerAsync
      , terminationReason = "forced restart"
      }
    cancelWith workerAsync RestartWorkerException

restartWorker
  :: CapatazEnv -> WorkerSpec -> WorkerId -> RestartCount -> IO Worker
restartWorker capatazEnv workerSpec workerId restartCount =
  Worker.forkWorker capatazEnv workerSpec (Just (workerId, restartCount))

--------------------------------------------------------------------------------

handleWorkerCompleted :: CapatazEnv -> WorkerId -> UTCTime -> IO ()
handleWorkerCompleted env@CapatazEnv { capatazName, capatazId, notifyEvent } workerId eventTime
  = do
    mWorkerEnv <- fetchWorkerEnv env workerId
    case mWorkerEnv of
      Nothing -> return ()
      Just workerEnv@WorkerEnv { workerName, workerAsync, workerRestartStrategy } ->
        do
          notifyEvent SupervisedWorkerCompleted
            { capatazId
            , capatazName
            , workerId
            , workerName
            , eventTime
            , workerThreadId  = asyncThreadId workerAsync
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

handleWorkerFailed :: CapatazEnv -> WorkerId -> SomeException -> Int -> IO ()
handleWorkerFailed env@CapatazEnv { capatazName, capatazId, notifyEvent } workerId workerError restartCount
  = do
    mWorkerEnv <- fetchWorkerEnv env workerId
    case mWorkerEnv of
      Nothing -> return ()
      Just workerEnv@WorkerEnv { workerName, workerAsync, workerRestartStrategy } ->
        do
          eventTime <- getCurrentTime
          notifyEvent SupervisedWorkerFailed
            { capatazName
            , capatazId
            , workerId
            , workerName
            , workerError
            , workerThreadId  = asyncThreadId workerAsync
            , eventTime
            }
          case workerRestartStrategy of
            Temporary -> removeWorkerFromMap env workerId
            _         -> execRestartAction env workerEnv restartCount

handleWorkerTerminated :: CapatazEnv -> WorkerId -> Text -> Int -> IO ()
handleWorkerTerminated env@CapatazEnv { capatazName, capatazId, notifyEvent } workerId terminationReason workerRestartCount
  = do
    mWorkerEnv <- fetchWorkerEnv env workerId
    case mWorkerEnv of
      Nothing -> return ()
      Just workerEnv@WorkerEnv { workerName, workerAsync, workerRestartStrategy } ->
        do
          eventTime <- getCurrentTime
          notifyEvent SupervisedWorkerTerminated
            { capatazName
            , capatazId
            , workerId
            , workerName
            , eventTime
            , terminationReason
            , workerThreadId     = asyncThreadId workerAsync
            }
          case workerRestartStrategy of
            Permanent -> execRestartAction env workerEnv workerRestartCount
            _         -> removeWorkerFromMap env workerId

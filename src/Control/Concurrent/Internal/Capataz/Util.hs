{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
module Control.Concurrent.Internal.Capataz.Util where

import Protolude

import           Control.Concurrent.STM        (STM, atomically, retry)
import           Control.Concurrent.STM.TQueue (writeTQueue)
import           Control.Concurrent.STM.TVar   (TVar, readTVar, writeTVar)
import           Data.IORef                    (atomicModifyIORef', readIORef)
import qualified Data.Text                     as T
import           Data.Time.Clock               (getCurrentTime)

import qualified Data.HashMap.Strict as HashMap

import Control.Concurrent.Internal.Capataz.Types

getTidNumber :: ThreadId -> Maybe Text
getTidNumber tid = case T.words $ show tid of
  (_:tidNumber:_) -> Just tidNumber
  _               -> Nothing

--------------------------------------------------------------------------------

fetchWorker :: CapatazEnv -> WorkerId -> IO (Maybe Worker)
fetchWorker CapatazEnv { capatazWorkerMap } workerId =
  HashMap.lookup workerId <$> readIORef capatazWorkerMap

fetchWorkerEnv :: CapatazEnv -> WorkerId -> IO (Maybe WorkerEnv)
fetchWorkerEnv CapatazEnv { capatazWorkerMap } workerId =
  ((workerToEnv <$>) . HashMap.lookup workerId) <$> readIORef capatazWorkerMap

appendWorkerToMap :: CapatazEnv -> Worker -> IO ()
appendWorkerToMap CapatazEnv { capatazWorkerMap } worker@Worker { workerId } =
  atomicModifyIORef' capatazWorkerMap
                     (\workerMap -> (appendWorker workerMap, ()))
  where appendWorker = HashMap.alter (const $ Just worker) workerId

removeWorkerFromMap :: CapatazEnv -> WorkerId -> IO ()
removeWorkerFromMap CapatazEnv { capatazWorkerMap } workerId =
  atomicModifyIORef'
    capatazWorkerMap
    ( \workerMap -> maybe (workerMap, ())
                         (const (HashMap.delete workerId workerMap, ()))
                         (HashMap.lookup workerId workerMap)
    )

resetWorkerMap :: CapatazEnv -> (WorkerMap -> WorkerMap) -> IO ()
resetWorkerMap CapatazEnv { capatazWorkerMap } workerMapFn =
  atomicModifyIORef' capatazWorkerMap (\workerMap -> (workerMapFn workerMap, ()))

readWorkerMap :: CapatazEnv -> IO WorkerMap
readWorkerMap CapatazEnv { capatazWorkerMap } =
  readIORef capatazWorkerMap

sortWorkersByTerminationOrder :: WorkerTerminationOrder -> WorkerMap -> [Worker]
sortWorkersByTerminationOrder terminationOrder workerMap =
  case terminationOrder of
    OldestFirst -> workers
    NewestFirst -> reverse workers
 where
    -- NOTE: dissambiguates workerCreationTime field
  workerCreationTime' Worker { workerCreationTime } = workerCreationTime

  workers = sortBy (comparing workerCreationTime') (HashMap.elems workerMap)

--------------------------------------------------------------------------------

readCapatazStatusSTM :: TVar CapatazStatus -> STM CapatazStatus
readCapatazStatusSTM statusVar = do
  status <- readTVar statusVar
  if status == Initializing then retry else return status

readCapatazStatus :: CapatazEnv -> IO CapatazStatus
readCapatazStatus CapatazEnv { capatazStatusVar } =
  atomically $ readTVar capatazStatusVar

writeCapatazStatus :: CapatazEnv -> CapatazStatus -> IO ()
writeCapatazStatus CapatazEnv { capatazId, capatazName, capatazStatusVar, notifyEvent } newCapatazStatus
  = do

    prevCapatazStatus <- atomically $ do
      prevStatus <- readTVar capatazStatusVar
      writeTVar capatazStatusVar newCapatazStatus
      return prevStatus

    eventTime <- getCurrentTime
    notifyEvent CapatazStatusChanged
      { capatazId
      , capatazName
      , prevCapatazStatus
      , newCapatazStatus
      , eventTime
      }


sendControlMsg :: CapatazEnv -> ControlAction -> IO ()
sendControlMsg CapatazEnv { capatazQueue } ctrlMsg =
  atomically $ writeTQueue capatazQueue (ControlAction ctrlMsg)

sendSyncControlMsg :: CapatazEnv -> (IO () -> ControlAction) -> IO ()
sendSyncControlMsg CapatazEnv { capatazQueue } mkCtrlMsg = do
  result <- newEmptyMVar
  atomically $ writeTQueue capatazQueue
                           (ControlAction $ mkCtrlMsg (putMVar result ()))
  takeMVar result

capatazToEnv :: CapatazRuntime -> CapatazEnv
capatazToEnv capatazRuntime@CapatazRuntime {..} =
  let CapatazOptions {..} = capatazOptions in CapatazEnv {..}

workerToEnv :: Worker -> WorkerEnv
workerToEnv Worker {..} =
  let
    WorkerSpec { workerAction, workerOnFailure, workerOnCompletion, workerOnTermination, workerRestartStrategy }
      = workerSpec
  in
    WorkerEnv {..}

envToWorker :: WorkerEnv -> Worker
envToWorker WorkerEnv {..} = Worker {..}

workerOptionsToSpec :: WorkerOptions -> IO () -> WorkerSpec
workerOptionsToSpec WorkerOptions {..} workerAction = WorkerSpec {..}

capatazToAsync :: Capataz -> Async ()
capatazToAsync = capatazAsync

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

-- | Returns only the number of the ThreadId
getTidNumber :: ThreadId -> Maybe Text
getTidNumber tid = case T.words $ show tid of
  (_:tidNumber:_) -> Just tidNumber
  _               -> Nothing

--------------------------------------------------------------------------------

-- | Fetches a "Worker" from the "Capataz" instance environment
fetchWorker :: CapatazEnv -> WorkerId -> IO (Maybe Worker)
fetchWorker CapatazEnv { capatazWorkerMap } workerId =
  HashMap.lookup workerId <$> readIORef capatazWorkerMap

-- | Fetches a "WorkerEnv" from the "Capataz" instance environment
fetchWorkerEnv :: CapatazEnv -> WorkerId -> IO (Maybe WorkerEnv)
fetchWorkerEnv CapatazEnv { capatazWorkerMap } workerId =
  ((workerToEnv <$>) . HashMap.lookup workerId) <$> readIORef capatazWorkerMap

-- | Appends a new "Worker" to the "Capataz" existing worker map.
appendWorkerToMap :: CapatazEnv -> Worker -> IO ()
appendWorkerToMap CapatazEnv { capatazWorkerMap } worker@Worker { workerId } =
  atomicModifyIORef' capatazWorkerMap
                     (\workerMap -> (appendWorker workerMap, ()))
  where appendWorker = HashMap.alter (const $ Just worker) workerId

-- | Removes a "Worker" from the "Capataz" existing worker map.
removeWorkerFromMap :: CapatazEnv -> WorkerId -> IO ()
removeWorkerFromMap CapatazEnv { capatazWorkerMap } workerId =
  atomicModifyIORef'
    capatazWorkerMap
    ( \workerMap -> maybe (workerMap, ())
                         (const (HashMap.delete workerId workerMap, ()))
                         (HashMap.lookup workerId workerMap)
    )

-- | Function to modify a "Capataz" worker map using a pure function.
resetWorkerMap :: CapatazEnv -> (WorkerMap -> WorkerMap) -> IO ()
resetWorkerMap CapatazEnv { capatazWorkerMap } workerMapFn =
  atomicModifyIORef' capatazWorkerMap (\workerMap -> (workerMapFn workerMap, ()))

-- | Function to get a snapshot of the "Capataz"' worker map
readWorkerMap :: CapatazEnv -> IO WorkerMap
readWorkerMap CapatazEnv { capatazWorkerMap } =
  readIORef capatazWorkerMap

-- | Returns all worker's of a "Capataz" by "WorkerTerminationOrder". This is
-- used "AllForOne" restarts and shutdown operations.
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

-- | Sub-routine that returns the "CapatazStatus", this sub-routine will block
-- until the "Capataz" has a status different from "Initializing".
readCapatazStatusSTM :: TVar CapatazStatus -> STM CapatazStatus
readCapatazStatusSTM statusVar = do
  status <- readTVar statusVar
  if status == Initializing then retry else return status

-- | Sub-routine that returns the "CapatazStatus" on the IO monad
readCapatazStatus :: CapatazEnv -> IO CapatazStatus
readCapatazStatus CapatazEnv { capatazStatusVar } =
  atomically $ readTVar capatazStatusVar

-- | Modifes the "Capataz" status, this is the only function that should be used
-- to this end given it has the side-effect of notifying a status change via the
-- "notifyEvent" sub-routine, given via an attribute of the "CapatazOption"
-- record.
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


-- | Used from public API functions to send a ControlAction to the Capataz
-- supervisor thread loop
sendControlMsg :: CapatazEnv -> ControlAction -> IO ()
sendControlMsg CapatazEnv { capatazQueue } ctrlMsg =
  atomically $ writeTQueue capatazQueue (ControlAction ctrlMsg)

-- | Used from public API functions to send a ControlAction to the Capataz
-- supervisor thread loop, it receives an IO sub-routine that expects an IO
-- operation that blocks a thread until the message is done.
sendSyncControlMsg
  :: CapatazEnv
  -> (IO () -> ControlAction) -- ^ Blocking sub-routine used from the caller
  -> IO ()
sendSyncControlMsg CapatazEnv { capatazQueue } mkCtrlMsg = do
  result <- newEmptyMVar
  atomically $ writeTQueue capatazQueue
                           (ControlAction $ mkCtrlMsg (putMVar result ()))
  takeMVar result

-- | Utility function to transform a "CapatazRuntime" into a "CapatazEnv"
capatazToEnv :: CapatazRuntime -> CapatazEnv
capatazToEnv capatazRuntime@CapatazRuntime {..} =
  let CapatazOptions {..} = capatazOptions in CapatazEnv {..}

-- | Utility function to transform a "Worker" into a "WorkerEnv"
workerToEnv :: Worker -> WorkerEnv
workerToEnv Worker {..} =
  let
    WorkerSpec { workerAction, workerOnFailure, workerOnCompletion, workerOnTermination, workerRestartStrategy }
      = workerSpec
  in
    WorkerEnv {..}

-- | Utility function to transform a "WorkerEnv" into a "Worker"
envToWorker :: WorkerEnv -> Worker
envToWorker WorkerEnv {..} = Worker {..}

-- | Utility function to transform a "WorkerOptions" into a "WorkerSpec"
workerOptionsToSpec :: WorkerOptions -> IO () -> WorkerSpec
workerOptionsToSpec WorkerOptions {..} workerAction = WorkerSpec {..}

-- | Utility function to transform a "Capataz" into an @"Async" ()@
capatazToAsync :: Capataz -> Async ()
capatazToAsync = capatazAsync

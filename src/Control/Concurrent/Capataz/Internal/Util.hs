{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
{-| This module contains:

* Functions to manipulate the state of the Capataz record
* Utility functions used for communication between threads
* Public API utility functions

-}
module Control.Concurrent.Capataz.Internal.Util where

import Protolude

import           Control.Concurrent.STM      (STM, atomically, retry)
import           Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import           Data.IORef                  (atomicModifyIORef', readIORef)
import qualified Data.Text                   as T
import           Data.Time.Clock             (getCurrentTime)

import qualified Data.HashMap.Strict as HashMap
import           GHC.Conc            (labelThread)

import Control.Concurrent.Capataz.Internal.Types

-- | Returns only the number of the ThreadId
getTidNumber :: ThreadId -> Maybe Text
getTidNumber tid = case T.words $ show tid of
  (_:tidNumber:_) -> Just tidNumber
  _               -> Nothing

--------------------------------------------------------------------------------

-- | Internal functions that overwrites the GHC thread name, for increasing
-- traceability on GHC internals
setProcessThreadName :: WorkerId -> WorkerName -> IO ()
setProcessThreadName workerId workerName = do
  tid <- myThreadId
  let workerIdentifier =
        T.unpack workerName <> "_" <> show workerId <> "_" <> maybe
          ""
          T.unpack
          (getTidNumber tid)
  labelThread tid workerIdentifier


getProcessId :: Process -> ProcessId
getProcessId process = case process of
  WorkerProcess     Worker { workerId }         -> workerId
  SupervisorProcess Supervisor { supervisorId } -> supervisorId

-- | Fetches a "Worker" from the "Capataz" instance environment
fetchProcess :: SupervisorEnv -> ProcessId -> IO (Maybe Process)
fetchProcess SupervisorEnv { supervisorProcessMap } processId = do
  processMap <- readIORef supervisorProcessMap
  case HashMap.lookup processId processMap of
    Just process -> return $ Just process
    _            -> return Nothing

-- | Appends a new "Worker" to the "Capataz" existing worker map.
appendProcessToMap :: SupervisorEnv -> Process -> IO ()
appendProcessToMap SupervisorEnv { supervisorProcessMap } process =
  atomicModifyIORef' supervisorProcessMap
                     (\processMap -> (appendProcess processMap, ()))
 where
  appendProcess = HashMap.alter (const $ Just process) (getProcessId process)

-- | Removes a "Worker" from the "Capataz" existing worker map.
removeProcessFromMap :: SupervisorEnv -> ProcessId -> IO ()
removeProcessFromMap SupervisorEnv { supervisorProcessMap } processId =
  atomicModifyIORef'
    supervisorProcessMap
    ( \processMap -> maybe (processMap, ())
                           (const (HashMap.delete processId processMap, ()))
                           (HashMap.lookup processId processMap)
    )

-- | Function to modify a "Capataz" worker map using a pure function.
resetProcessMap :: SupervisorEnv -> (ProcessMap -> ProcessMap) -> IO ()
resetProcessMap SupervisorEnv { supervisorProcessMap } processMapFn =
  atomicModifyIORef' supervisorProcessMap
                     (\processMap -> (processMapFn processMap, ()))

-- | Function to get a snapshot of the "Capataz"' worker map
readProcessMap :: SupervisorEnv -> IO ProcessMap
readProcessMap SupervisorEnv { supervisorProcessMap } =
  readIORef supervisorProcessMap

-- | Returns all worker's of a "Capataz" by "ProcessTerminationOrder". This is
-- used "AllForOne" restarts and shutdown operations.
sortProcessesByTerminationOrder
  :: ProcessTerminationOrder -> ProcessMap -> [Process]
sortProcessesByTerminationOrder terminationOrder processMap =
  case terminationOrder of
    OldestFirst -> workers
    NewestFirst -> reverse workers
 where
    -- NOTE: dissambiguates workerCreationTime field
  processCreationTime (WorkerProcess Worker { workerCreationTime }) =
    workerCreationTime
  processCreationTime (SupervisorProcess Supervisor { supervisorCreationTime })
    = supervisorCreationTime

  workers = sortBy (comparing processCreationTime) (HashMap.elems processMap)

--------------------------------------------------------------------------------

-- | Sub-routine that returns the "SupervisorStatus", this sub-routine will block
-- until the "Capataz" has a status different from "Initializing".
readSupervisorStatusSTM :: TVar SupervisorStatus -> STM SupervisorStatus
readSupervisorStatusSTM statusVar = do
  status <- readTVar statusVar
  if status == Initializing then retry else return status

-- | Sub-routine that returns the "SupervisorStatus" on the IO monad
readSupervisorStatus :: SupervisorEnv -> IO SupervisorStatus
readSupervisorStatus SupervisorEnv { supervisorStatusVar } =
  atomically $ readTVar supervisorStatusVar

-- | Modifes the "Capataz" status, this is the only function that should be used
-- to this end given it has the side-effect of notifying a status change via the
-- "notifyEvent" sub-routine, given via an attribute of the "CapatazOption"
-- record.
writeSupervisorStatus :: SupervisorEnv -> SupervisorStatus -> IO ()
writeSupervisorStatus SupervisorEnv { supervisorId, supervisorName, supervisorStatusVar, notifyEvent } newSupervisorStatus
  = do

    prevSupervisorStatus <- atomically $ do
      prevStatus <- readTVar supervisorStatusVar
      writeTVar supervisorStatusVar newSupervisorStatus
      return prevStatus

    eventTime <- getCurrentTime
    notifyEvent SupervisorStatusChanged
      { supervisorId         = supervisorId
      , supervisorName       = supervisorName
      , prevSupervisorStatus
      , newSupervisorStatus
      , eventTime
      }


-- | Used from public API functions to send a ControlAction to the Capataz
-- supervisor thread loop
sendControlMsg :: SupervisorEnv -> ControlAction -> IO ()
sendControlMsg SupervisorEnv { supervisorNotify } ctrlMsg =
  supervisorNotify (ControlAction ctrlMsg)

-- | Used from public API functions to send a ControlAction to the Capataz
-- supervisor thread loop, it receives an IO sub-routine that expects an IO
-- operation that blocks a thread until the message is done.
sendSyncControlMsg
  :: SupervisorEnv
  -> (IO () -> ControlAction) -- ^ Blocking sub-routine used from the caller
  -> IO ()
sendSyncControlMsg SupervisorEnv { supervisorNotify } mkCtrlMsg = do
  result <- newEmptyMVar
  supervisorNotify (ControlAction $ mkCtrlMsg (putMVar result ()))
  takeMVar result

capatazOptionsToSupervisorOptions :: CapatazOptions -> SupervisorOptions
capatazOptionsToSupervisorOptions CapatazOptions {..} = SupervisorOptions {..}

toParentSupervisorEnv :: SupervisorEnv -> ParentSupervisorEnv
toParentSupervisorEnv SupervisorEnv { supervisorId, supervisorName, supervisorNotify, notifyEvent }
  = ParentSupervisorEnv {..}

capatazToAsync :: Capataz -> Async ()
capatazToAsync Capataz { capatazSupervisor } =
  let Supervisor { supervisorAsync } = capatazSupervisor in supervisorAsync

supervisorToAsync :: Supervisor -> Async ()
supervisorToAsync Supervisor { supervisorAsync } = supervisorAsync

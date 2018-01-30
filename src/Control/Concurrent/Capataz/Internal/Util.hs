{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-| This module contains:

* Functions to manipulate the state of the Supervisor record
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

-- | Returns only the number of the ThreadId.
getTidNumber :: ThreadId -> Maybe Text
getTidNumber tid = case T.words $ show tid of
  (_:tidNumber:_) -> Just tidNumber
  _               -> Nothing

--------------------------------------------------------------------------------

-- | Internal functions that overwrites the GHC thread name, for increasing
-- traceability on GHC internals.
setProcessThreadName :: WorkerId -> WorkerName -> IO ()
setProcessThreadName workerId workerName = do
  tid <- myThreadId
  let workerIdentifier =
        T.unpack workerName <> "_" <> show workerId <> "_" <> maybe
          ""
          T.unpack
          (getTidNumber tid)
  labelThread tid workerIdentifier

-- | Gets the "ProcessId" of both a Worker or Supervisor process.
getProcessId :: Process -> ProcessId
getProcessId process = case process of
  WorkerProcess     Worker { workerId }         -> workerId
  SupervisorProcess Supervisor { supervisorId } -> supervisorId

-- | Gets a supervised "Process" from a "Supervisor" instance.
fetchProcess :: SupervisorEnv -> ProcessId -> IO (Maybe Process)
fetchProcess SupervisorEnv { supervisorProcessMap } processId = do
  processMap <- readIORef supervisorProcessMap
  case HashMap.lookup processId processMap of
    Just process -> return $ Just process
    _            -> return Nothing

-- | Appends a new "Process" to the "Supervisor" existing process map.
appendProcessToMap :: SupervisorEnv -> Process -> IO ()
appendProcessToMap SupervisorEnv { supervisorProcessMap } process =
  atomicModifyIORef' supervisorProcessMap
                     (\processMap -> (appendProcess processMap, ()))
 where
  appendProcess = HashMap.alter (const $ Just process) (getProcessId process)

-- | Removes a "Process" from a "Supervisor" existing process map.
removeProcessFromMap :: SupervisorEnv -> ProcessId -> IO ()
removeProcessFromMap SupervisorEnv { supervisorProcessMap } processId =
  atomicModifyIORef'
    supervisorProcessMap
    ( \processMap -> maybe (processMap, ())
                           (const (HashMap.delete processId processMap, ()))
                           (HashMap.lookup processId processMap)
    )

-- | Function to modify a "Supervisor" process map using a pure function.
resetProcessMap :: SupervisorEnv -> (ProcessMap -> ProcessMap) -> IO ()
resetProcessMap SupervisorEnv { supervisorProcessMap } processMapFn =
  atomicModifyIORef' supervisorProcessMap
                     (\processMap -> (processMapFn processMap, ()))

-- | Function to get a snapshot of a "Supervisor" process map.
readProcessMap :: SupervisorEnv -> IO ProcessMap
readProcessMap SupervisorEnv { supervisorProcessMap } =
  readIORef supervisorProcessMap

-- | Returns all processes of a "Supervisor" by "ProcessTerminationOrder". This
-- is used on "AllForOne" restarts and shutdown operations.
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

-- | Returns the "SupervisorStatus", this sub-routine will retry transaction
-- until its associated "Supervisor" has a status different from "Initializing".
readSupervisorStatusSTM :: TVar SupervisorStatus -> STM SupervisorStatus
readSupervisorStatusSTM statusVar = do
  status <- readTVar statusVar
  if status == Initializing then retry else return status

-- | Executes transaction that returns the "SupervisorStatus".
readSupervisorStatus :: SupervisorEnv -> IO SupervisorStatus
readSupervisorStatus SupervisorEnv { supervisorStatusVar } =
  atomically $ readTVar supervisorStatusVar

-- | Modifes the "Supervisor" status.
--
-- IMPORTANT: this is the only function that should be used for this purpose
-- given it has the side-effect of notifying a status change via the
-- "notifyEvent" sub-routine, orginally given in the "CapatazOption" record.
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

-- | Used from public API functions to send "ControlAction" messages to a
-- Supervisor thread loop.
sendControlMsg :: SupervisorEnv -> ControlAction -> IO ()
sendControlMsg SupervisorEnv { supervisorNotify } ctrlMsg =
  supervisorNotify (ControlAction ctrlMsg)

-- | Used from public API functions to send ControlAction messages to the a
-- Supervisor thread loop, it receives an IO sub-routine that expects an IO
-- operation that blocks a thread until the message is done.
sendSyncControlMsg
  :: SupervisorEnv
  -> (IO () -> ControlAction) -- ^ Blocking sub-routine used from the caller
  -> IO ()
sendSyncControlMsg SupervisorEnv { supervisorNotify } mkCtrlMsg = do
  result <- newEmptyMVar
  supervisorNotify (ControlAction $ mkCtrlMsg (putMVar result ()))
  takeMVar result

-- | Utility function to transform a "CapatazOptions" record to a
-- "SupervisorOptions" record.
capatazOptionsToSupervisorOptions :: CapatazOptions -> SupervisorOptions
capatazOptionsToSupervisorOptions CapatazOptions {..} =
  SupervisorOptions {supervisorName = "capataz-root-supervisor", ..}

-- | Utility function to transform a "SupervisorEnv" record to a
-- "ParentSupervisorEnv" record; used on functions where supervision of
-- supervisors is managed.
toParentSupervisorEnv :: SupervisorEnv -> ParentSupervisorEnv
toParentSupervisorEnv SupervisorEnv { supervisorId, supervisorName, supervisorNotify, notifyEvent }
  = ParentSupervisorEnv {..}

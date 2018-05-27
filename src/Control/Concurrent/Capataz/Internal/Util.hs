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

import           RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.List    as List
import qualified RIO.Text    as T

import RIO.Time (getCurrentTime)

import GHC.Conc (labelThread)

import Control.Concurrent.Capataz.Internal.Types

-- | Returns only the number of the ThreadId.
getTidNumber :: ThreadId -> Maybe Text
getTidNumber tid = case T.words $ tshow tid of
  (_ : tidNumber : _) -> Just tidNumber
  _                   -> Nothing

--------------------------------------------------------------------------------

-- | Internal functions that overwrites the GHC thread name, for increasing
-- traceability on GHC internals.
setProcessThreadName :: MonadIO m => WorkerId -> WorkerName -> m ()
setProcessThreadName workerId workerName = do
  tid <- myThreadId
  let workerIdentifier =
        T.unpack workerName <> "_" <> show workerId <> "_" <> maybe
          ""
          T.unpack
          (getTidNumber tid)
  liftIO $ labelThread tid workerIdentifier

-- | Gets the 'ProcessId' of both a Worker or Supervisor process.
getProcessId :: Process m -> ProcessId
getProcessId process = case process of
  WorkerProcess     Worker { workerId }         -> workerId
  SupervisorProcess Supervisor { supervisorId } -> supervisorId

-- | Gets a supervised 'Process' from a 'Supervisor' instance.
fetchProcess
  :: MonadIO m => SupervisorEnv m -> ProcessId -> m (Maybe (Process m))
fetchProcess SupervisorEnv { supervisorProcessMap } processId = do
  processMap <- readIORef supervisorProcessMap
  case HashMap.lookup processId processMap of
    Just process -> return $ Just process
    _            -> return Nothing

-- | Appends a new 'Process' to the 'Supervisor' existing process map.
appendProcessToMap :: MonadIO m => SupervisorEnv m -> Process m -> m ()
appendProcessToMap SupervisorEnv { supervisorProcessMap } process =
  atomicModifyIORef' supervisorProcessMap
                     (\processMap -> (appendProcess processMap, ()))
 where
  appendProcess = HashMap.alter (const $ Just process) (getProcessId process)

-- | Removes a 'Process' from a 'Supervisor' existing process map.
removeProcessFromMap :: MonadIO m => SupervisorEnv m -> ProcessId -> m ()
removeProcessFromMap SupervisorEnv { supervisorProcessMap } processId =
  atomicModifyIORef'
    supervisorProcessMap
    (\processMap -> maybe (processMap, ())
                          (const (HashMap.delete processId processMap, ()))
                          (HashMap.lookup processId processMap)
    )

-- | Function to modify a 'Supervisor' process map using a pure function.
resetProcessMap
  :: MonadIO m => SupervisorEnv m -> (ProcessMap m -> ProcessMap m) -> m ()
resetProcessMap SupervisorEnv { supervisorProcessMap } processMapFn =
  atomicModifyIORef' supervisorProcessMap
                     (\processMap -> (processMapFn processMap, ()))

-- | Function to get a snapshot of a 'Supervisor' process map.
readProcessMap :: MonadIO m => SupervisorEnv m -> m (ProcessMap m)
readProcessMap SupervisorEnv { supervisorProcessMap } =
  readIORef supervisorProcessMap

-- | Returns all processes of a 'Supervisor' by 'ProcessTerminationOrder'. This
-- is used on 'AllForOne' restarts and shutdown operations.
sortProcessesByTerminationOrder
  :: Monad m => ProcessTerminationOrder -> ProcessMap m -> [Process m]
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

  workers =
    List.sortBy (comparing processCreationTime) (HashMap.elems processMap)

--------------------------------------------------------------------------------

-- | Returns the 'SupervisorStatus', this sub-routine will retry transaction
-- until its associated 'Supervisor' has a status different from 'Initializing'.
readSupervisorStatusSTM :: TVar SupervisorStatus -> STM SupervisorStatus
readSupervisorStatusSTM statusVar = do
  status <- readTVar statusVar
  if status == Initializing then retrySTM else return status

-- | Executes transaction that returns the 'SupervisorStatus'.
readSupervisorStatus :: MonadIO m => SupervisorEnv m -> m SupervisorStatus
readSupervisorStatus SupervisorEnv { supervisorStatusVar } =
  atomically $ readTVar supervisorStatusVar

-- | Modifes the 'Supervisor' status.
--
-- __IMPORTANT__ This is the only function that should be used for this purpose
-- given it has the side-effect of notifying a status change via the
-- 'notifyEvent' sub-routine, orginally given in the 'CapatazOption' record.
writeSupervisorStatus
  :: MonadIO m => SupervisorEnv m -> SupervisorStatus -> m ()
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

-- | Used from public API functions to send 'ControlAction' messages to a
-- Supervisor thread loop.
sendControlMsg :: MonadIO m => SupervisorEnv m -> ControlAction m -> m ()
sendControlMsg SupervisorEnv { supervisorNotify } ctrlMsg =
  supervisorNotify (ControlAction ctrlMsg)

-- | Used from public API functions to send ControlAction messages to the a
-- Supervisor thread loop, it receives an IO sub-routine that expects an IO
-- operation that blocks a thread until the message is done.
sendSyncControlMsg
  :: MonadIO m
  => SupervisorEnv m
  -> (m () -> ControlAction m) -- ^ Blocking sub-routine used from the caller
  -> m ()
sendSyncControlMsg SupervisorEnv { supervisorNotify } mkCtrlMsg = do
  result <- newEmptyMVar
  supervisorNotify (ControlAction $ mkCtrlMsg (putMVar result ()))
  takeMVar result

-- | Utility function to transform a 'CapatazOptions' record to a
-- 'SupervisorOptions' record.
capatazOptionsToSupervisorOptions
  :: Monad m => CapatazOptions m -> SupervisorOptions m
capatazOptionsToSupervisorOptions CapatazOptions {..} = SupervisorOptions {..}

-- | Utility function to transform a 'SupervisorEnv' record to a
-- 'ParentSupervisorEnv' record; used on functions where supervision of
-- supervisors is managed.
toParentSupervisorEnv :: Monad m => SupervisorEnv m -> ParentSupervisorEnv m
toParentSupervisorEnv SupervisorEnv { supervisorId, supervisorName, supervisorNotify, notifyEvent }
  = ParentSupervisorEnv {..}

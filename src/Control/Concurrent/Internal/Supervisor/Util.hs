{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE RecordWildCards          #-}
module Control.Concurrent.Internal.Supervisor.Util where

import Protolude

import Control.Concurrent.STM      (atomically, STM, retry)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Data.IORef                  (atomicModifyIORef')
import Data.Time.Clock             (getCurrentTime)

import qualified Data.HashMap.Strict as HashMap

import Control.Concurrent.Internal.Supervisor.Types

appendChildToMap :: SupervisorEnv -> ChildId -> Child -> IO ()
appendChildToMap SupervisorEnv { supervisorChildMap } childId child =
  atomicModifyIORef' supervisorChildMap
                     (\childMap -> (appendChild childMap, ()))
  where appendChild = HashMap.alter (const $ Just child) childId

removeChildFromMap :: SupervisorEnv -> ChildId -> (Child -> IO ()) -> IO ()
removeChildFromMap SupervisorEnv { supervisorChildMap } childId withChild = do
  mChild <- atomicModifyIORef' supervisorChildMap
            (\childMap ->
                maybe (childMap, Nothing)
                (\child -> (HashMap.delete childId childMap, Just child))
                (HashMap.lookup childId childMap))
  maybe (return ()) withChild mChild

readSupervisorStatus :: TVar SupervisorStatus -> STM SupervisorStatus
readSupervisorStatus statusVar = do
  status <- readTVar statusVar
  if status == Initializing then retry else return status

writeSupervisorStatus :: SupervisorEnv -> SupervisorStatus -> IO ()
writeSupervisorStatus SupervisorEnv { supervisorId, supervisorName, supervisorStatusVar, notifyEvent } newSupervisorStatus
  = do

    prevSupervisorStatus <- atomically $ do
      prevStatus <- readTVar supervisorStatusVar
      writeTVar supervisorStatusVar newSupervisorStatus
      return prevStatus

    eventTime <- getCurrentTime
    notifyEvent SupervisorStatusChanged
      { supervisorId
      , supervisorName
      , prevSupervisorStatus
      , newSupervisorStatus
      , eventTime
      }

sendControlMsg :: SupervisorEnv -> ControlAction -> IO ()
sendControlMsg SupervisorEnv {supervisorQueue} ctrlMsg =
  atomically $ writeTQueue
    supervisorQueue
    (ControlAction ctrlMsg)

sendSyncControlMsg :: SupervisorEnv -> (IO () -> ControlAction) -> IO ()
sendSyncControlMsg SupervisorEnv {supervisorQueue} mkCtrlMsg = do
  result <- newEmptyMVar
  atomically $ writeTQueue
    supervisorQueue
    (ControlAction $ mkCtrlMsg (putMVar result ()))
  takeMVar result

runtimeToEnv :: SupervisorRuntime -> SupervisorEnv
runtimeToEnv supervisorRuntime@SupervisorRuntime {..} =
  let SupervisorSpec {..} = supervisorSpec in SupervisorEnv {..}

childOptionsToSpec :: ChildOptions -> IO () -> ChildSpec
childOptionsToSpec ChildOptions {..} childAction =
  ChildSpec {..}

{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE RecordWildCards          #-}
module Control.Concurrent.Internal.Supervisor.Util where

import Protolude

import Control.Concurrent.STM        (STM, atomically, retry)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Concurrent.STM.TVar   (TVar, readTVar, writeTVar)
import Data.IORef                    (atomicModifyIORef', readIORef)
import Data.Time.Clock               (getCurrentTime)

import qualified Data.HashMap.Strict as HashMap

import Control.Concurrent.Internal.Supervisor.Types

withChildEnv :: SupervisorEnv -> ChildId -> (ChildEnv -> IO ()) -> IO ()
withChildEnv SupervisorEnv { supervisorChildMap } childId withChildFn = do
  childMap <- readIORef supervisorChildMap
  maybe (return ()) (withChildFn . childToEnv) (HashMap.lookup childId childMap)


appendChildToMap :: SupervisorEnv -> ChildId -> Child -> IO ()
appendChildToMap SupervisorEnv { supervisorChildMap } childId child =
  atomicModifyIORef' supervisorChildMap
                     (\childMap -> (appendChild childMap, ()))
  where appendChild = HashMap.alter (const $ Just child) childId

removeChildFromMap :: SupervisorEnv -> ChildId -> (ChildEnv -> IO ()) -> IO ()
removeChildFromMap SupervisorEnv { supervisorChildMap } childId withChildFn = do
  mChild <- atomicModifyIORef'
    supervisorChildMap
    ( \childMap -> maybe (childMap, Nothing)
                         (\child -> (HashMap.delete childId childMap, Just child))
                         (HashMap.lookup childId childMap)
    )
  maybe (putText $ "ChildId not found: " <> show childId) (withChildFn . childToEnv) mChild

readSupervisorStatusSTM :: TVar SupervisorStatus -> STM SupervisorStatus
readSupervisorStatusSTM statusVar = do
  status <- readTVar statusVar
  if status == Initializing then retry else return status

readSupervisorStatus :: SupervisorEnv -> IO SupervisorStatus
readSupervisorStatus SupervisorEnv {supervisorStatusVar} =
  atomically $ readTVar supervisorStatusVar

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

resetChildMap :: SupervisorEnv -> IO ChildMap
resetChildMap (SupervisorEnv {supervisorChildMap}) =
  atomicModifyIORef' supervisorChildMap (\childMap -> (HashMap.empty, childMap))

sortChildrenByTerminationOrder
  :: ChildTerminationOrder
  -> ChildMap
  -> [Child]
sortChildrenByTerminationOrder terminationOrder childMap =
    case terminationOrder of
      OldestFirst ->
        children
      NewestFirst ->
        reverse children
  where
    -- NOTE: dissambiguates childCreationTime field
    childCreationTime' (Child {childCreationTime}) =
      childCreationTime

    children =
      sortBy (comparing childCreationTime')
             (HashMap.elems childMap)


sendControlMsg :: SupervisorEnv -> ControlAction -> IO ()
sendControlMsg SupervisorEnv { supervisorQueue } ctrlMsg =
  atomically $ writeTQueue supervisorQueue (ControlAction ctrlMsg)

sendSyncControlMsg :: SupervisorEnv -> (IO () -> ControlAction) -> IO ()
sendSyncControlMsg SupervisorEnv { supervisorQueue } mkCtrlMsg = do
  result <- newEmptyMVar
  atomically $ writeTQueue supervisorQueue
                           (ControlAction $ mkCtrlMsg (putMVar result ()))
  takeMVar result

supervisorToEnv :: SupervisorRuntime -> SupervisorEnv
supervisorToEnv supervisorRuntime@SupervisorRuntime {..} =
  let SupervisorOptions {..} = supervisorOptions in SupervisorEnv {..}

childToEnv :: Child -> ChildEnv
childToEnv Child {..} =
  let ChildSpec {childAction,
                 childOnFailure,
                 childOnCompletion,
                 childOnTermination,
                 childRestartStrategy} = childSpec in ChildEnv {..}

childOptionsToSpec :: ChildOptions -> IO () -> ChildSpec
childOptionsToSpec ChildOptions {..} childAction =
  ChildSpec {..}

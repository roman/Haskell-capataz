{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
module Control.Concurrent.Internal.Supervisor.Util where

import Protolude

import           Control.Concurrent.STM        (STM, atomically, retry)
import           Control.Concurrent.STM.TQueue (writeTQueue)
import           Control.Concurrent.STM.TVar   (TVar, readTVar, writeTVar)
import           Data.IORef                    (atomicModifyIORef', readIORef)
import qualified Data.Text                     as T
import           Data.Time.Clock               (getCurrentTime)

import qualified Data.HashMap.Strict as HashMap

import Control.Concurrent.Internal.Supervisor.Types

getTidNumber :: ThreadId -> Maybe Text
getTidNumber tid = case T.words $ show tid of
  (_:tidNumber:_) -> Just tidNumber
  _               -> Nothing

--------------------------------------------------------------------------------

fetchChild :: SupervisorEnv -> ChildId -> IO (Maybe Child)
fetchChild SupervisorEnv { supervisorChildMap } childId =
  HashMap.lookup childId <$> readIORef supervisorChildMap

fetchChildEnv :: SupervisorEnv -> ChildId -> IO (Maybe ChildEnv)
fetchChildEnv SupervisorEnv { supervisorChildMap } childId =
  ((childToEnv <$>) . HashMap.lookup childId) <$> readIORef supervisorChildMap

appendChildToMap :: SupervisorEnv -> Child -> IO ()
appendChildToMap SupervisorEnv { supervisorChildMap } child@Child { childId } =
  atomicModifyIORef' supervisorChildMap
                     (\childMap -> (appendChild childMap, ()))
  where appendChild = HashMap.alter (const $ Just child) childId

removeChildFromMap :: SupervisorEnv -> ChildId -> IO ()
removeChildFromMap SupervisorEnv { supervisorChildMap } childId =
  atomicModifyIORef'
    supervisorChildMap
    ( \childMap -> maybe (childMap, ())
                         (const (HashMap.delete childId childMap, ()))
                         (HashMap.lookup childId childMap)
    )

resetChildMap :: SupervisorEnv -> (ChildMap -> ChildMap) -> IO ()
resetChildMap SupervisorEnv { supervisorChildMap } childMapFn =
  atomicModifyIORef' supervisorChildMap (\childMap -> (childMapFn childMap, ()))

readChildMap :: SupervisorEnv -> IO ChildMap
readChildMap SupervisorEnv { supervisorChildMap } =
  readIORef supervisorChildMap

sortChildrenByTerminationOrder :: ChildTerminationOrder -> ChildMap -> [Child]
sortChildrenByTerminationOrder terminationOrder childMap =
  case terminationOrder of
    OldestFirst -> children
    NewestFirst -> reverse children
 where
    -- NOTE: dissambiguates childCreationTime field
  childCreationTime' Child { childCreationTime } = childCreationTime

  children = sortBy (comparing childCreationTime') (HashMap.elems childMap)

--------------------------------------------------------------------------------

readSupervisorStatusSTM :: TVar SupervisorStatus -> STM SupervisorStatus
readSupervisorStatusSTM statusVar = do
  status <- readTVar statusVar
  if status == Initializing then retry else return status

readSupervisorStatus :: SupervisorEnv -> IO SupervisorStatus
readSupervisorStatus SupervisorEnv { supervisorStatusVar } =
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
  let
    ChildSpec { childAction, childOnFailure, childOnCompletion, childOnTermination, childRestartStrategy }
      = childSpec
  in
    ChildEnv {..}

envToChild :: ChildEnv -> Child
envToChild ChildEnv {..} = Child {..}

childOptionsToSpec :: ChildOptions -> IO () -> ChildSpec
childOptionsToSpec ChildOptions {..} childAction = ChildSpec {..}

supervisorToAsync :: Supervisor -> Async ()
supervisorToAsync = supervisorAsync

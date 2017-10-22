{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE RecordWildCards          #-}
module Control.Concurrent.Internal.Supervisor.Util where

import Protolude

import Control.Concurrent.STM      (STM, retry)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Data.IORef                  (atomicModifyIORef')
import Data.Time.Clock             (getCurrentTime)

import qualified Data.HashMap.Strict as H

import Control.Concurrent.Internal.Supervisor.Types

appendChildToMap :: SupervisorEnv -> ChildId -> Child -> IO ()
appendChildToMap (SupervisorEnv {supervisorChildMap}) childId child =
    atomicModifyIORef' supervisorChildMap
                       (\childMap -> (appendChild childMap, ()))
  where
    appendChild = H.alter (const $ Just child) childId

readSupervisorStatus :: TVar SupervisorStatus -> STM SupervisorStatus
readSupervisorStatus statusVar = do
  status <- readTVar statusVar
  if status == Initializing then
    retry
  else
    return status

writeSupervisorStatus :: SupervisorEnv -> SupervisorStatus -> IO ()
writeSupervisorStatus (SupervisorEnv { supervisorId
                                     , supervisorName
                                     , supervisorStatusVar
                                     , notifyEvent})
                    newSupervisorStatus = do

  prevSupervisorStatus <- atomically $ do
    prevStatus <- readTVar supervisorStatusVar
    writeTVar supervisorStatusVar newSupervisorStatus
    return prevStatus

  eventTime <- getCurrentTime
  notifyEvent (SupervisorStatusChanged { supervisorId
                                       , supervisorName
                                       , prevSupervisorStatus
                                       , newSupervisorStatus
                                       , eventTime })

runtimeToEnv :: SupervisorRuntime -> SupervisorEnv
runtimeToEnv supervisorRuntime@(SupervisorRuntime {..}) =
  let
    (SupervisorSpec {..}) =
      supervisorSpec
  in
    SupervisorEnv {..}

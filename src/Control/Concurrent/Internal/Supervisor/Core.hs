{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Control.Concurrent.Internal.Supervisor.Core where

import Protolude

import Control.Concurrent.MVar       (newEmptyMVar, takeMVar)
import Control.Concurrent.STM        (atomically)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar   (newTVarIO)
import Data.IORef                    (newIORef)
import Data.Time.Clock               (getCurrentTime)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.V4        as UUID (nextRandom)

import qualified Control.Concurrent.Internal.Supervisor.Child as Child

import Control.Concurrent.Internal.Supervisor.Types
import Control.Concurrent.Internal.Supervisor.Util
    (readSupervisorStatus, runtimeToEnv, writeSupervisorStatus)

--------------------------------------------------------------------------------

handleMonitorEvent :: SupervisorEnv -> MonitorEvent -> IO ()
handleMonitorEvent = panic "pending implementation"

handleControlAction :: SupervisorEnv -> ControlAction -> IO ()
handleControlAction env controlAction = case controlAction of
  ForkChild { childSpec, returnChildId } -> do
    childId <- Child.forkChild env childSpec Nothing Nothing
    returnChildId childId

  TerminateChild{} -> panic "pending implementation"

handleSupervisorMessage :: SupervisorEnv -> SupervisorMessage -> IO ()
handleSupervisorMessage env message = case message of
  ControlAction controlAction -> handleControlAction env controlAction

  MonitorEvent  monitorEvent  -> handleMonitorEvent env monitorEvent

runSupervisorLoop :: SupervisorEnv -> IO ()
runSupervisorLoop env@SupervisorEnv { supervisorId, supervisorName, supervisorStatusVar, supervisorQueue, notifyEvent }
  = do
    (status, message) <-
      atomically
      $   (,)
      <$> readSupervisorStatus supervisorStatusVar
      <*> readTQueue supervisorQueue

    case status of
      Initializing -> do
        eventTime <- getCurrentTime
        notifyEvent InvalidSupervisorStatusReached
          { supervisorId
          , supervisorName
          , eventTime
          }

      Running -> do
        handleSupervisorMessage env message
        runSupervisorLoop env

      Halting -> panic "pending implementation"

      Halted  -> panic "pending implementation"

buildSupervisorRuntime :: SupervisorSpec -> IO SupervisorRuntime
buildSupervisorRuntime supervisorSpec = do
  supervisorId        <- UUID.nextRandom
  supervisorQueue     <- newTQueueIO
  supervisorStatusVar <- newTVarIO Initializing
  supervisorChildMap  <- newIORef HashMap.empty
  return SupervisorRuntime {..}

forkSupervisor :: SupervisorSpec -> IO Supervisor
forkSupervisor supervisorSpec = do
  supervisorRuntime <- buildSupervisorRuntime supervisorSpec

  let supervisorEnv = runtimeToEnv supervisorRuntime

  supervisorAsync <- async $ runSupervisorLoop supervisorEnv

  writeSupervisorStatus supervisorEnv Running

  return Supervisor {..}

forkChild :: ChildSpec -> Supervisor -> IO ChildId
forkChild childSpec Supervisor { supervisorEnv } = do
  let SupervisorEnv { supervisorQueue } = supervisorEnv

  childIdVar <- newEmptyMVar
  atomically $ writeTQueue
    supervisorQueue
    (ControlAction ForkChild
      { childSpec
      , childRestartCount = 0
      , returnChildId     = putMVar childIdVar
      })
  takeMVar childIdVar

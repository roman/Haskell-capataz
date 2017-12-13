{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
module Control.Concurrent.Supervisor.Internal.Core
  ( forkChild
  , forkSupervisor
  , stopSupervisor
  ) where

import Protolude

import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue)
import Control.Concurrent.STM.TVar   (TVar, newTVarIO, readTVar, writeTVar)
import Data.IORef                    (newIORef)
import Data.Time.Clock               (getCurrentTime)

import qualified Data.HashMap.Strict as H
import qualified Data.UUID.V4        as UUID

import Control.Concurrent.Supervisor.Internal.Child   (forkChild, terminateChildren)
import Control.Concurrent.Supervisor.Internal.Restart (ingestChildMonitorEvent)
import Control.Concurrent.Supervisor.Internal.Types
import Control.Concurrent.Supervisor.Internal.Util    (removeChild, toSupervisorEnv)

--------------------------------------------------------------------------------
--

readSupervisorStatus :: TVar SupervisorStatus -> STM SupervisorStatus
readSupervisorStatus statusRef = do
  status <- readTVar statusRef
  case status of
      Initializing ->
          retry
      _ -> do
          return status

monitorChildId :: MonitorEvent  -> ChildId
monitorChildId event =
  case event of
    Failed {childId} ->
      childId
    Finished {childId} ->
      childId
    Terminated {childId} ->
      childId

-- Iterate over termination events from children threads and executes
-- an appropriate restart procedure
supervisorLoop :: SupervisorEnv -> IO ()
supervisorLoop env@(SupervisorEnv { supervisorName
                                  , supervisorId
                                  , supervisorStatusVar
                                  , supervisorEventQueue
                                  , notifyEvent
                                  }) = do

  (status, event) <- atomically
       $ (,)
       <$> readSupervisorStatus supervisorStatusVar
       <*> readTQueue supervisorEventQueue

  case status of
    Initializing -> do
      eventTime <- getCurrentTime
      let
        errorMessage =
          "Event Loop execution on inconsistent state; supervisor not initialized."
      notifyEvent (SupervisorStateError {supervisorName, supervisorId, eventTime, errorMessage})
      panic errorMessage

    Halted -> do
      -- Finish of supervisor loop
      -- TODO: Consume all pending elements from queue
      eventTime <- getCurrentTime
      notifyEvent (SupervisorHalted {supervisorName, supervisorId, eventTime})
      return ()

    Halting ->
      -- ignore all events till SupervisorHalted
      supervisorLoop env

    Running -> do
      -- normal behavior here
      removeChild env (monitorChildId event) (flip ingestChildMonitorEvent event)
      supervisorLoop env

specToRuntime :: SupervisorOptions -> IO SupervisorRuntime
specToRuntime supervisorOptions = do
  supervisorId         <- UUID.nextRandom
  supervisorChildMap   <- newIORef H.empty
  supervisorStatusVar  <- newTVarIO Initializing
  supervisorEventQueue <- newTQueueIO
  return
    $ SupervisorRuntime {
      supervisorId
    , supervisorChildMap
    , supervisorEventQueue
    , supervisorStatusVar
    , supervisorOptions
    }

--------------------------------------------------------------------------------
-- Public API

-- | Creates a supervisor green thread that monitors children threads
-- spawned from this supervisor
forkSupervisor :: SupervisorOptions -> IO Supervisor
forkSupervisor spec = do
  supervisorRuntime <- specToRuntime spec
  supervisorAsync <- async $ supervisorLoop (toSupervisorEnv supervisorRuntime)
  return $ Supervisor {..}

-- | Stops supervisor and monitored children threads
stopSupervisor :: Supervisor -> IO ()
stopSupervisor Supervisor {supervisorRuntime, supervisorAsync} = do
  let
    supervisorEnv@(SupervisorEnv { supervisorStatusVar
                                 , supervisorShutdownTimeoutSeconds
                                 }) =
      toSupervisorEnv supervisorRuntime

  atomically $ writeTVar supervisorStatusVar Halting
  terminateChildren supervisorEnv "stopSupervisor"
  atomically $ writeTVar supervisorStatusVar Halted
  race_ (threadDelay $ supervisorShutdownTimeoutSeconds * 1000100)
        (wait supervisorAsync)

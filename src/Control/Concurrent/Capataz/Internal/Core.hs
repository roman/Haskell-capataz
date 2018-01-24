{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-| This module contains:

* Functions exported on the public API
* High level message handlers of the supervisor thread loop

-}
module Control.Concurrent.Capataz.Internal.Core where

import Protolude

import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Teardown        (newTeardown)
import Control.Teardown        (Teardown)
import Data.Time.Clock         (getCurrentTime)

import qualified Data.UUID.V4 as UUID (nextRandom)

import qualified Control.Concurrent.Capataz.Internal.Supervisor as Supervisor

import           Control.Concurrent.Capataz.Internal.Types
import qualified Control.Concurrent.Capataz.Internal.Util  as Util

--------------------------------------------------------------------------------

-- | Utility typeclass to call public supervision API with types
-- that contain a supervisor (e.g. Capataz record)
class HasSupervisor a where
  -- | Fetches a supervisor from a record internals
  getSupervisor :: a -> Supervisor

instance HasSupervisor Capataz where
  getSupervisor (Capataz {capatazSupervisor}) = capatazSupervisor

instance HasSupervisor Supervisor where
  getSupervisor = identity

-- | Creates a Capataz record, which holds a root Supervisor; this Supervisor
-- monitors failures on process threads defined in the "CapatazOptions" or
-- created dynamically using "forkWorker" or "forkSupervisor".
forkCapataz :: CapatazOptions -> IO Capataz
forkCapataz capatazOptions@CapatazOptions { notifyEvent } = do
  capatazId    <- UUID.nextRandom
  supervisorId <- UUID.nextRandom
  let
    supervisorOptions@SupervisorOptions { supervisorName } =
      Util.capatazOptionsToSupervisorOptions capatazOptions
    parentSupervisorEnv = ParentSupervisorEnv
      { supervisorId     = capatazId
      , supervisorName   = "capataz-root"
      , supervisorNotify = \supervisorEvent -> do
        eventTime <- getCurrentTime
        case supervisorEvent of
          MonitorEvent ProcessFailed' { processError } -> notifyEvent
            CapatazFailed
              { supervisorId
              , supervisorName
              , eventTime
              , supervisorError = processError
              }

          MonitorEvent ProcessTerminated'{} -> notifyEvent CapatazTerminated
            { supervisorId
            , supervisorName
            , eventTime
            }

          MonitorEvent ProcessCompleted'{} ->
            panic "Capataz completed; this should never happen"

          MonitorEvent ProcessForcedRestart{} ->
            panic
              "Capataz was restarted from a OneForAll strategy; this should never happen"

          ControlAction{} ->
            panic "Capataz received a ControlAction message; bad implementation"
      , notifyEvent
      }

  capatazSupervisor@Supervisor { supervisorEnv } <- Supervisor.supervisorMain
    parentSupervisorEnv
    supervisorOptions
    supervisorId
    0

  capatazTeardown <- newTeardown
    "capataz"
    ( do
      Supervisor.haltSupervisor "capataz teardown" supervisorEnv
      eventTime <- getCurrentTime
      notifyEvent CapatazTerminated {supervisorId , supervisorName , eventTime }
    )

  return Capataz {capatazSupervisor , capatazTeardown }

-- | Creates a worker green thread "IO ()" sub-routine, and depending in options
-- defined in the "WorkerOptions" record, it will restart the Worker sub-routine
-- in case of failures
forkWorker
  :: HasSupervisor supervisor
  => WorkerOptions -- ^ Worker options (restart, name, callbacks, etc)
  -> supervisor    -- ^ "Supervisor" that supervises the worker
  -> IO WorkerId   -- ^ An identifier that can be used to terminate the "Worker"
forkWorker workerOptions sup = do
  let Supervisor { supervisorNotify } = getSupervisor sup
  workerIdVar <- newEmptyMVar
  supervisorNotify
    ( ControlAction ForkWorker
      { workerOptions
      , returnWorkerId = putMVar workerIdVar
      }
    )
  takeMVar workerIdVar

-- | Creates a Supervisor, which can create other processes.
forkSupervisor
  :: HasSupervisor parentSupervisor
  => SupervisorOptions -- ^ Supervisor options
  -> parentSupervisor  -- ^ Parent supervisor instance that supervises new supervisor
  -> IO Supervisor     -- ^ A Supervisor record to dynamically create and
                       -- supervise other processes
forkSupervisor supervisorOptions parentSup = do
  let Supervisor { supervisorNotify } = getSupervisor parentSup
  supervisorVar <- newEmptyMVar
  supervisorNotify
    ( ControlAction ForkSupervisor
      { supervisorOptions
      , returnSupervisor  = putMVar supervisorVar
      }
    )
  takeMVar supervisorVar

-- | Stops the execution of a process green thread being supervised by the given
-- "Supervisor" instance, if the "ProcessId" does not belong to the given
-- Supervisor, the operation does not perform any side-effect.
--
-- Note: If ProcessId maps to a worker that has a "Permanent" restart strategy,
-- the worker thread __will be restarted again__; so use a "Transient" restart
-- strategy for the worker instead.
terminateProcess
  :: HasSupervisor supervisor => Text -> ProcessId -> supervisor -> IO ()
terminateProcess processTerminationReason processId supervisor = do
  let Supervisor { supervisorNotify } = getSupervisor supervisor
  result <- newEmptyMVar
  supervisorNotify
    ( ControlAction TerminateProcess
      { processId
      , processTerminationReason
      , notifyProcessTermination = putMVar result ()
      }
    )
  takeMVar result

getCapatazTeardown :: Capataz -> Teardown
getCapatazTeardown Capataz { capatazTeardown } = capatazTeardown

-- | Gets the async of a Supervisor thread
getSupervisorAsync :: HasSupervisor supervisor => supervisor -> Async ()
getSupervisorAsync supervisor =
  let Supervisor { supervisorAsync } = getSupervisor supervisor
  in  supervisorAsync

-- | Gets the process identifier of a Supervisor (normally used for termination)
getSupervisorProcessId :: Supervisor -> ProcessId
getSupervisorProcessId Supervisor { supervisorId } = supervisorId

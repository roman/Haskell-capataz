{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-| This module contains:

* Functions exported on the public API
* The supervisor thread loop
* High level message handlers of the supervisor thread loop

-}
module Control.Concurrent.Capataz.Internal.Core where

import Protolude

import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Teardown        (newTeardown)
import Data.Time.Clock         (getCurrentTime)

import qualified Data.UUID.V4 as UUID (nextRandom)

-- import qualified Control.Concurrent.Capataz.Internal.Restart as Restart
-- import qualified Control.Concurrent.Capataz.Internal.Worker  as Worker
-- import qualified Control.Concurrent.Capataz.Internal.Process as Process
import qualified Control.Concurrent.Capataz.Internal.Supervisor as Supervisor

import           Control.Concurrent.Capataz.Internal.Types
import qualified Control.Concurrent.Capataz.Internal.Util  as Util

--------------------------------------------------------------------------------

-- | Creates a Capataz record, which represents a supervision thread which
-- monitors failure on worker threads defined in the "CapatazOptions" or worker
-- threads that are created dynamically using "forkWorker".
forkCapataz :: CapatazOptions -> IO Capataz
forkCapataz capatazOptions@CapatazOptions { notifyEvent } = do
  capatazId    <- UUID.nextRandom
  supervisorId <- UUID.nextRandom
  let
    supervisorSpec@SupervisorSpec { supervisorName } =
      Util.capatazOptionsToSupervisorSpec capatazOptions
    parentSupervisorEnv = ParentSupervisorEnv
      { supervisorId     = capatazId
      , supervisorName   = "capataz-root"
      , supervisorNotify = \supervisorEvent -> do
        eventTime <- getCurrentTime
        case supervisorEvent of
          MonitorEvent ProcessFailed' { processError } -> do
            notifyEvent CapatazFailed
              { supervisorId
              , supervisorName
              , eventTime
              , supervisorError = processError
              }

          MonitorEvent ProcessTerminated'{} -> do
            notifyEvent CapatazTerminated
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
    supervisorSpec
    supervisorId
    0

  capatazTeardown <- newTeardown
    "capataz"
    ( do
      Supervisor.haltSupervisor "capataz teardown" supervisorEnv
      eventTime <- getCurrentTime
      notifyEvent CapatazTerminated {supervisorId , supervisorName , eventTime }
    )

  return Capataz {..}

-- | Creates a worker green thread "IO ()" sub-routine, and depending in options
-- defined in the "WorkerOptions" record, it will restart the Worker sub-routine
-- in case of failures
forkWorker
  :: WorkerOptions -- ^ Worker options (restart, name, callbacks, etc)
  -> IO ()         -- ^ IO sub-routine that will be executed on worker thread
  -> Capataz       -- ^ "Capataz" instance that supervises the worker
  -> IO WorkerId   -- ^ An identifier that can be used to terminate the "Worker"
forkWorker workerOptions workerAction Capataz { capatazSupervisor = supervisor }
  = do
    let workerSpec = Util.workerOptionsToSpec workerOptions workerAction
        Supervisor { supervisorNotify } = supervisor

    workerIdVar <- newEmptyMVar
    supervisorNotify
      ( ControlAction ForkWorker
        { workerSpec
        , returnWorkerId = putMVar workerIdVar
        }
      )
    takeMVar workerIdVar

-- | Stops the execution of a worker green thread being supervised by the given
-- "Capataz" instance, if the WorkerId does not belong to the Capataz, the
-- operation does not perform any side-effect.
--
-- Note: If your worker has a "Permanent" worker restart strategy, the worker
-- thread __will be restarted again__; so use a "Transient" restart strategy
-- instead.
terminateProcess :: Text -> ProcessId -> Capataz -> IO ()
terminateProcess processTerminationReason processId Capataz { capatazSupervisor = supervisor }
  = do
    let Supervisor { supervisorNotify } = supervisor
    result <- newEmptyMVar
    supervisorNotify
      ( ControlAction $ TerminateProcess
        { processId
        , processTerminationReason
        , notifyProcessTermination = putMVar result ()
        }
      )
    takeMVar result

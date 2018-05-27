{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

{-| This module contains:

* Functions exported on the public API
* High level message handlers of the supervisor thread loop

-}
module Control.Concurrent.Capataz.Internal.Core
(
  HasSupervisor(..)
, forkWorker
, forkSupervisor
, forkCapataz
, terminateProcess
, terminateCapataz
, terminateCapataz_
, joinCapatazThread
, getSupervisorProcessId
, getSupervisorAsync
, getCapatazTeardown
)
where

import RIO

import Control.Teardown (Teardown, TeardownResult, newTeardown, runTeardown, runTeardown_)
import RIO.Time         (getCurrentTime)

import qualified Data.UUID.V4 as UUID (nextRandom)

import qualified Control.Concurrent.Capataz.Internal.Supervisor as Supervisor

import           Control.Concurrent.Capataz.Internal.Types
import qualified Control.Concurrent.Capataz.Internal.Util  as Util

--------------------------------------------------------------------------------

-- | Utility typeclass to call public supervision API with types
-- that contain a supervisor (e.g. Capataz record).
class HasSupervisor a where
  -- | Fetches a supervisor from a record internals.
  getSupervisor :: a m -> Supervisor m

instance HasSupervisor Capataz where
  getSupervisor Capataz {capatazSupervisor} = capatazSupervisor

instance HasSupervisor Supervisor where
  getSupervisor = id

-- | Creates a Capataz record, which holds both a root supervisor and a
-- 'Teardown' to shut down the system. The root supervisor monitors failures on
-- process threads defined with 'supervisorProcessSpecList' or created
-- dynamically using 'forkWorker' or 'forkSupervisor'.
forkCapataz
  :: (MonadUnliftIO m, MonadIO m)
  => Text
  -> (CapatazOptions m -> CapatazOptions m)
  -> m (Capataz m)
forkCapataz capatazName modOptionsFn = do
  capatazId    <- liftIO UUID.nextRandom
  supervisorId <- liftIO UUID.nextRandom
  let
    capatazOptions@CapatazOptions { notifyEvent } =
      defCapatazOptions capatazName modOptionsFn
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
            error "Capataz completed; this should never happen"

          MonitorEvent ProcessForcedRestart{} ->
            error
              "Capataz was restarted from a OneForAll strategy; this should never happen"

          ControlAction{} ->
            error "Capataz received a ControlAction message; bad implementation"
      , notifyEvent
      }

  capatazSupervisor@Supervisor { supervisorEnv } <- Supervisor.supervisorMain
    parentSupervisorEnv
    supervisorOptions
    supervisorId
    0 -- initial restart count

  capatazTeardown <- withRunInIO $ \run -> newTeardown
    "capataz"
    (run $ do
      Supervisor.haltSupervisor "capataz system shutdown" supervisorEnv
      eventTime <- getCurrentTime
      notifyEvent CapatazTerminated {supervisorId , supervisorName , eventTime }
    )

  return Capataz {capatazSupervisor , capatazTeardown }

-- | Creates a green thread from an @IO ()@ sub-routine. Depending in options
-- defined in the 'WorkerOptions' record, it will automatically restart this
-- sub-routine in case of failures.
--
-- See documentation of related functions:
--
-- * 'buildWorkerOptionsWithDefaults'
-- * 'buildWorkerOptions'
--
forkWorker
  :: (MonadIO m, HasSupervisor supervisor)
  => WorkerOptions m -- ^ Worker options (restart, name, callbacks, etc)
  -> supervisor m   -- ^ 'Supervisor' that supervises the worker
  -> m WorkerId   -- ^ An identifier that can be used to terminate the 'Worker'
forkWorker workerOptions sup = do
  let Supervisor { supervisorNotify } = getSupervisor sup
  workerIdVar <- newEmptyMVar
  supervisorNotify
    (ControlAction ForkWorker
      { workerOptions
      , returnWorkerId = putMVar workerIdVar
      }
    )
  takeMVar workerIdVar

-- | Creates a green thread which monitors other green threads for failures and
-- restarts them using settings defined on 'SupervisorOptions'.
--
-- See documentation of related functions:
--
-- * 'buildSupervisorOptionsWithDefault'
-- * 'buildSupervisorOptions'
--
forkSupervisor
  :: (MonadIO m, HasSupervisor parentSupervisor)
  => SupervisorOptions m -- ^ Supervisor options
  -> parentSupervisor m  -- ^ Parent supervisor instance that supervises new supervisor
  -> m (Supervisor m)     -- ^ A record used to dynamically create and supervise
                       -- other processes
forkSupervisor supervisorOptions parentSup = do
  let Supervisor { supervisorNotify } = getSupervisor parentSup
  supervisorVar <- newEmptyMVar
  supervisorNotify
    (ControlAction ForkSupervisor
      { supervisorOptions
      , returnSupervisor  = putMVar supervisorVar
      }
    )
  takeMVar supervisorVar

-- | Stops the execution of a green thread being supervised by the given
-- supervisor.
--
-- __IMPORTANT__ If 'ProcessId' maps to a worker that is configured with a
-- 'Permanent' worker restart strategy, the worker green thread __will be
-- restarted again__.
--
terminateProcess
  :: (MonadIO m, HasSupervisor supervisor)
  => Text
  -> ProcessId
  -> supervisor m
  -> m Bool
terminateProcess processTerminationReason processId supervisor = do
  let Supervisor { supervisorNotify } = getSupervisor supervisor
  result <- newEmptyMVar
  supervisorNotify
    (ControlAction TerminateProcess
      { processId
      , processTerminationReason
      , notifyProcessTermination = putMVar result
      }
    )
  takeMVar result

-- | Joins the thread of the root supervisor of the given capataz system to the
-- current thread.
joinCapatazThread :: MonadIO m => Capataz m -> m ()
joinCapatazThread Capataz { capatazSupervisor } =
  let Supervisor { supervisorAsync } = capatazSupervisor
  in  wait supervisorAsync

-- | Terminates a 'Capataz' system (all supervised threads) and returns a 'TeardownResult'
--
-- @since 0.2.0.0
terminateCapataz :: MonadIO m => Capataz m -> m TeardownResult
terminateCapataz = liftIO . runTeardown

-- | Terminates a 'Capataz' system (all supervised threads)
--
-- @since 0.2.0.0
terminateCapataz_ :: MonadIO m => Capataz m -> m ()
terminateCapataz_ = liftIO . runTeardown_

-- | Gets 'Teardown' record of this capataz system.
getCapatazTeardown :: Capataz m -> Teardown
getCapatazTeardown Capataz { capatazTeardown } = capatazTeardown

-- | Gets the 'Async' of a Supervisor thread.
--
-- NOTE: There is no way to get the 'Async' value of the root supervisor; this
-- is done on-purpose to avoid error scenarios.
getSupervisorAsync :: Supervisor m -> Async ()
getSupervisorAsync Supervisor { supervisorAsync } = supervisorAsync

-- | Gets the process identifier of a 'Supervisor'; normally used for termination.
getSupervisorProcessId :: Supervisor m -> ProcessId
getSupervisorProcessId Supervisor { supervisorId } = supervisorId

{-# LANGUAGE NoImplicitPrelude #-}
{-| Public API for the capataz library

    Capataz is a library that brings an OTP-like supervisor API to the Haskell
    concurrency toolset.

-}
module Control.Concurrent.Capataz
(
-- * Types
  HasSupervisor (..)
, CallbackType (..)

, WorkerId
, WorkerAction
, WorkerRestartStrategy (..)
, WorkerTerminationPolicy (..)
, WorkerOptions

, ProcessId
, ProcessSpec (..)
, ProcessType (..)
, ProcessTerminationOrder (..)
, ProcessError (..)

, SupervisorId
, Supervisor
, SupervisorRestartStrategy (..)
, SupervisorStatus (..)
, SupervisorOptions

, CapatazOptions

, Capataz

-- * Default Options for Capataz Processes
, defSupervisorOptions
, defWorkerOptions
, defCapatazOptions

-- * Lenses to modify Option Records
, supervisorNameL
, supervisorIntensityL
, supervisorPeriodSecondsL
, supervisorRestartStrategyL
, supervisorProcessSpecListL
, supervisorProcessTerminationOrderL
, supervisorOnIntensityReachedL
, supervisorOnFailureL
, notifyEventL
, workerActionL
, workerNameL
, workerOnFailureL
, workerOnCompletionL
, workerOnTerminationL
, workerTerminationPolicyL
, workerRestartStrategyL

-- * Core functionality
, forkWorker
, forkCapataz
, terminateProcess

-- * Utility functions
, getSupervisorProcessId
, getSupervisorAsync
-- * Teardown (re-exported)
, teardown
-- * Lens (re-exported)
, (^.)
, (.~)
, (&)
, view
, set
)
where

import Control.Concurrent.Capataz.Internal.Core
    (HasSupervisor (..), forkCapataz, forkWorker, getSupervisorProcessId, getSupervisorAsync, terminateProcess)

import Control.Concurrent.Capataz.Internal.Types
    (
      CallbackType (..)

    , WorkerId
    , WorkerAction
    , WorkerRestartStrategy (..)
    , WorkerTerminationPolicy (..)
    , WorkerOptions

    , ProcessId
    , ProcessSpec (..)
    , ProcessType (..)
    , ProcessTerminationOrder (..)
    , ProcessError (..)

    , SupervisorId
    , Supervisor
    , SupervisorRestartStrategy (..)
    , SupervisorStatus (..)
    , SupervisorOptions

    , CapatazOptions

    , Capataz

    , defSupervisorOptions
    , defWorkerOptions
    , defCapatazOptions
    )

import Control.Concurrent.Capataz.Lens
  (
    supervisorNameL
  , supervisorIntensityL
  , supervisorPeriodSecondsL
  , supervisorRestartStrategyL
  , supervisorProcessSpecListL
  , supervisorProcessTerminationOrderL
  , supervisorOnIntensityReachedL
  , supervisorOnFailureL
  , notifyEventL
  , workerActionL
  , workerNameL
  , workerOnFailureL
  , workerOnCompletionL
  , workerOnTerminationL
  , workerTerminationPolicyL
  , workerRestartStrategyL
  , (^.)
  , (.~)
  , (&)
  , view
  , set
  )
import Control.Teardown                          (teardown)

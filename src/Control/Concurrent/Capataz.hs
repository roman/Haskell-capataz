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
, buildCapatazOptions
, buildSupervisorOptions
, buildWorkerOptions
, supervisorSpec
, workerSpec

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
    ( HasSupervisor (..)
    , forkCapataz
    , forkWorker
    , getSupervisorAsync
    , getSupervisorProcessId
    , terminateProcess
    )

import Control.Concurrent.Capataz.Internal.Types
    ( CallbackType (..)
    , Capataz
    , CapatazOptions
    , ProcessError (..)
    , ProcessId
    , ProcessSpec (..)
    , ProcessTerminationOrder (..)
    , ProcessType (..)
    , Supervisor
    , SupervisorId
    , SupervisorOptions
    , SupervisorRestartStrategy (..)
    , SupervisorStatus (..)
    , WorkerAction
    , WorkerId
    , WorkerOptions
    , WorkerRestartStrategy (..)
    , WorkerTerminationPolicy (..)
    , buildCapatazOptions
    , buildSupervisorOptions
    , buildWorkerOptions
    , supervisorSpec
    , workerSpec
    )

import Control.Concurrent.Capataz.Lens
    ( notifyEventL
    , set
    , supervisorIntensityL
    , supervisorNameL
    , supervisorOnFailureL
    , supervisorOnIntensityReachedL
    , supervisorPeriodSecondsL
    , supervisorProcessSpecListL
    , supervisorProcessTerminationOrderL
    , supervisorRestartStrategyL
    , view
    , workerActionL
    , workerNameL
    , workerOnCompletionL
    , workerOnFailureL
    , workerOnTerminationL
    , workerRestartStrategyL
    , workerTerminationPolicyL
    , (&)
    , (.~)
    , (^.)
    )
import Control.Teardown                (teardown)

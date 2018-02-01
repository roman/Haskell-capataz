{-# LANGUAGE NoImplicitPrelude #-}
{-| Public API for the capataz library

    Capataz is a library that brings an OTP-like supervisor API to the Haskell
    concurrency toolset.

-}
module Control.Concurrent.Capataz
(
-- * Types
  Control.Concurrent.Capataz.Internal.Core.HasSupervisor (..)
, Control.Concurrent.Capataz.Internal.Types.CallbackType (..)

, Control.Concurrent.Capataz.Internal.Types.WorkerId
, Control.Concurrent.Capataz.Internal.Types.WorkerRestartStrategy (..)
, Control.Concurrent.Capataz.Internal.Types.WorkerTerminationPolicy (..)
, Control.Concurrent.Capataz.Internal.Types.WorkerOptions

, Control.Concurrent.Capataz.Internal.Types.ProcessId
, Control.Concurrent.Capataz.Internal.Types.ProcessSpec (..)
, Control.Concurrent.Capataz.Internal.Types.ProcessType (..)
, Control.Concurrent.Capataz.Internal.Types.ProcessTerminationOrder (..)
, Control.Concurrent.Capataz.Internal.Types.ProcessError (..)

, Control.Concurrent.Capataz.Internal.Types.SupervisorId
, Control.Concurrent.Capataz.Internal.Types.Supervisor
, Control.Concurrent.Capataz.Internal.Types.SupervisorRestartStrategy (..)
, Control.Concurrent.Capataz.Internal.Types.SupervisorStatus (..)
, Control.Concurrent.Capataz.Internal.Types.SupervisorOptions

, Control.Concurrent.Capataz.Internal.Types.CapatazOptions

, Control.Concurrent.Capataz.Internal.Types.Capataz

-- * Default Options for Capataz Processes
, Control.Concurrent.Capataz.Internal.Types.buildSupervisorOptions
, Control.Concurrent.Capataz.Internal.Types.buildSupervisorOptionsWithDefaults
, Control.Concurrent.Capataz.Internal.Types.buildWorkerOptions
, Control.Concurrent.Capataz.Internal.Types.buildWorkerOptionsWithDefaults
, Control.Concurrent.Capataz.Internal.Types.supervisorSpec
, Control.Concurrent.Capataz.Internal.Types.supervisorSpecWithDefaults
, Control.Concurrent.Capataz.Internal.Types.workerSpec
, Control.Concurrent.Capataz.Internal.Types.workerSpecWithDefaults

-- * Lenses to modify Option Records
, Control.Concurrent.Capataz.Lens.onSystemEventL
, Control.Concurrent.Capataz.Lens.supervisorIntensityL
, Control.Concurrent.Capataz.Lens.supervisorPeriodSecondsL
, Control.Concurrent.Capataz.Lens.supervisorRestartStrategyL
, Control.Concurrent.Capataz.Lens.supervisorProcessSpecListL
, Control.Concurrent.Capataz.Lens.supervisorProcessTerminationOrderL
, Control.Concurrent.Capataz.Lens.supervisorOnIntensityReachedL
, Control.Concurrent.Capataz.Lens.supervisorOnFailureL
, Control.Concurrent.Capataz.Lens.workerOnFailureL
, Control.Concurrent.Capataz.Lens.workerOnCompletionL
, Control.Concurrent.Capataz.Lens.workerOnTerminationL
, Control.Concurrent.Capataz.Lens.workerTerminationPolicyL
, Control.Concurrent.Capataz.Lens.workerRestartStrategyL

-- * Core functionality
, Control.Concurrent.Capataz.Internal.Core.forkWorker
, Control.Concurrent.Capataz.Internal.Core.forkSupervisor
, Control.Concurrent.Capataz.Internal.Core.forkCapataz
, Control.Concurrent.Capataz.Internal.Core.terminateProcess

-- * Utility functions
, Control.Concurrent.Capataz.Internal.Core.joinCapatazThread
, Control.Concurrent.Capataz.Internal.Core.getSupervisorProcessId
, Control.Concurrent.Capataz.Internal.Core.getSupervisorAsync
, Control.Concurrent.Capataz.Internal.Core.getCapatazTeardown

-- * Teardown (re-exported)
, Control.Teardown.teardown

-- * Lens (re-exported)
, (.~)
, (&)
, Control.Concurrent.Capataz.Lens.set
)
where

import qualified Control.Concurrent.Capataz.Internal.Core
import qualified Control.Concurrent.Capataz.Internal.Types
import           Control.Concurrent.Capataz.Lens           ((&), (.~))
import qualified Control.Concurrent.Capataz.Lens
import qualified Control.Teardown

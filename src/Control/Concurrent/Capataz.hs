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
, WorkerAction
, ProcessError (..)
, WorkerOptions (..)
, WorkerRestartStrategy (..)
, SupervisorOptions (..)
, ProcessTerminationOrder (..)
, WorkerTerminationPolicy (..)
, Capataz
, CapatazEvent (..)
, CapatazOptions (..)
, Supervisor
, SupervisorRestartStrategy (..)
, SupervisorStatus (..)
, ProcessSpec (..)
, ProcessType (..)
, defSupervisorOptions
, defWorkerOptions
, defCapatazOptions
-- * Core functionality
, forkWorker
, forkCapataz
, terminateProcess
-- * Utility functions
, getSupervisorProcessId
, capatazToAsync
, supervisorToAsync
-- * Teardown (re-exported)
, teardown
)
where

import Control.Concurrent.Capataz.Internal.Core
    (HasSupervisor (..), forkCapataz, forkWorker, getSupervisorProcessId, terminateProcess)
import Control.Concurrent.Capataz.Internal.Types
    ( CallbackType (..)
    , Capataz
    , CapatazEvent (..)
    , CapatazOptions (..)
    , ProcessError (..)
    , ProcessSpec (..)
    , ProcessTerminationOrder (..)
    , ProcessType (..)
    , Supervisor
    , SupervisorOptions (..)
    , SupervisorRestartStrategy (..)
    , SupervisorStatus (..)
    , WorkerAction
    , WorkerOptions (..)
    , WorkerRestartStrategy (..)
    , WorkerTerminationPolicy (..)
    , defCapatazOptions
    , defSupervisorOptions
    , defWorkerOptions
    )
import Control.Concurrent.Capataz.Internal.Util  (capatazToAsync, supervisorToAsync)
import Control.Teardown                          (teardown)

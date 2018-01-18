{-# LANGUAGE NoImplicitPrelude #-}
{-| Public API for the capataz library

    Capataz is a library that brings an OTP-like supervisor API to the Haskell
    concurrency toolset.

-}
module Control.Concurrent.Capataz
(
-- * Types
  CallbackType (..)
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
, SupervisorRestartStrategy (..)
, CapatazStatus (..)
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
, capatazToAsync
-- * Teardown (re-exported)
, teardown
)
where

import Control.Concurrent.Capataz.Internal.Core  (forkCapataz, forkWorker, terminateProcess)
import Control.Concurrent.Capataz.Internal.Types
    ( CallbackType (..)
    , Capataz
    , CapatazEvent (..)
    , CapatazOptions (..)
    , CapatazStatus (..)
    , ProcessError (..)
    , ProcessSpec (..)
    , ProcessTerminationOrder (..)
    , ProcessType (..)
    , SupervisorOptions (..)
    , SupervisorRestartStrategy (..)
    , WorkerAction
    , WorkerOptions (..)
    , WorkerRestartStrategy (..)
    , WorkerTerminationPolicy (..)
    , defCapatazOptions
    , defSupervisorOptions
    , defWorkerOptions
    )
import Control.Concurrent.Capataz.Internal.Util  (capatazToAsync)
import Control.Teardown                          (teardown)

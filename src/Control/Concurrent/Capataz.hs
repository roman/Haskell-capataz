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
, WorkerSpec (..)
, ProcessTerminationOrder (..)
, WorkerTerminationPolicy (..)
, Capataz
, CapatazEvent (..)
, CapatazOptions (..)
, SupervisorRestartStrategy (..)
, CapatazStatus (..)
, ProcessSpec (..)
, defWorkerOptions
, defWorkerSpec
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
    , SupervisorRestartStrategy (..)
    , WorkerAction
    , WorkerOptions (..)
    , WorkerRestartStrategy (..)
    , WorkerSpec (..)
    , WorkerTerminationPolicy (..)
    , defCapatazOptions
    , defWorkerOptions
    , defWorkerSpec
    )
import Control.Concurrent.Capataz.Internal.Util  (capatazToAsync)
import Control.Teardown                          (teardown)

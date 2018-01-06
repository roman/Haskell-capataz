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
, WorkerError (..)
, WorkerOptions (..)
, WorkerRestartStrategy (..)
, WorkerSpec (..)
, ProcessTerminationOrder (..)
, WorkerTerminationPolicy (..)
, Capataz (..)
, CapatazEvent (..)
, CapatazOptions (..)
, CapatazRestartStrategy (..)
, CapatazStatus (..)
, ProcessSpec (..)
, defWorkerOptions
, defWorkerSpec
, defCapatazOptions
-- * Core functionality
, forkWorker
, forkCapataz
, terminateWorker
-- * Utility functions
, capatazToAsync
-- * Teardown (re-exported)
, teardown
)
where

import Control.Concurrent.Capataz.Internal.Core
    (forkCapataz, forkWorker, terminateWorker)
import Control.Concurrent.Capataz.Internal.Types
    ( CallbackType (..)
    , Capataz
    , CapatazEvent (..)
    , CapatazOptions (..)
    , CapatazRestartStrategy (..)
    , CapatazStatus (..)
    , ProcessSpec (..)
    , ProcessTerminationOrder (..)
    , WorkerAction
    , WorkerError (..)
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

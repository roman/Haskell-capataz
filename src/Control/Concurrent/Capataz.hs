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
, WorkerTerminationOrder (..)
, WorkerTerminationPolicy (..)
, Capataz (..)
, CapatazEvent (..)
, CapatazOptions (..)
, CapatazRestartStrategy (..)
, CapatazStatus (..)
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

import Control.Concurrent.Internal.Capataz.Core  (forkCapataz, forkWorker, terminateWorker)
import Control.Concurrent.Internal.Capataz.Types
    ( CallbackType (..)
    , Capataz (..)
    , CapatazEvent (..)
    , CapatazOptions (..)
    , CapatazRestartStrategy (..)
    , CapatazStatus (..)
    , WorkerAction
    , WorkerError (..)
    , WorkerOptions (..)
    , WorkerRestartStrategy (..)
    , WorkerSpec (..)
    , WorkerTerminationOrder (..)
    , WorkerTerminationPolicy (..)
    , defCapatazOptions
    , defWorkerOptions
    , defWorkerSpec
    )
import Control.Concurrent.Internal.Capataz.Util  (capatazToAsync)
import Control.Teardown                          (teardown)

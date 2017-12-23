{-# LANGUAGE NoImplicitPrelude #-}
module Control.Concurrent.Supervisor
( module Types
, module Core
, module Teardown
)
where

import Control.Concurrent.Internal.Supervisor.Core  as Core
    (forkChild, forkSupervisor, terminateChild)
import Control.Concurrent.Internal.Supervisor.Types as Types
    ( ChildAction
    , ChildOptions (..)
    , ChildRestartStrategy (..)
    , ChildSpec (..)
    , ChildTerminationOrder (..)
    , Supervisor (..)
    , SupervisorEvent (..)
    , SupervisorOptions (..)
    , SupervisorRestartStrategy (..)
    , SupervisorStatus (..)
    , defChildOptions
    , defChildSpec
    , defSupervisorOptions
    )
import Control.Teardown                             as Teardown (teardown)

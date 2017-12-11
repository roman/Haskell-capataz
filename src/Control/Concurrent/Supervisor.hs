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
    , ChildOptions
    , Supervisor
    , SupervisorEvent (..)
    , SupervisorSpec (..)
    , defChildOptions
    , defSupervisorSpec
    )
import Control.Teardown                             as Teardown (teardown)

{-# LANGUAGE NoImplicitPrelude #-}
module Control.Concurrent.Supervisor
( module Types
, module Core
, module Teardown
)
where

import Control.Concurrent.Internal.Supervisor.Core as Core (
    forkSupervisor
  , forkChild
  , terminateChild
  )
import Control.Concurrent.Internal.Supervisor.Types as Types (
    SupervisorSpec (..)
  , Supervisor
  , SupervisorEvent (..)
  , ChildOptions
  , ChildAction
  , defSupervisorSpec
  , defChildOptions
  )
import Control.Teardown as Teardown (teardown)

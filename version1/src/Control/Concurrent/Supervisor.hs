{-# LANGUAGE NoImplicitPrelude #-}
module Control.Concurrent.Supervisor where

import Protolude

import Control.Concurrent.Supervisor.Internal.Core
  (startSupervisor, stopSupervisor, forkChild, haltChild)

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Control.Concurrent.Internal.Supervisor.Types where

import Protolude

import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM.TVar   (TVar)
import Data.HashMap.Strict           (HashMap)
import Data.IORef                    (IORef)
import Data.Time.Clock               (UTCTime)
import Data.UUID                     (UUID)

type SupervisorId = UUID
type ChildId = UUID
type SupervisorName = Text
type ChildName = Text
type RestartCount = Int
type ChildMap = HashMap ChildId Child

data SupervisorEvent
  = InvalidSupervisorStatusReached {
    supervisorId   :: !SupervisorId
  , supervisorName :: !SupervisorName
  , eventTime      :: !UTCTime
  }
  | SupervisorStatusChanged {
    supervisorId         :: !SupervisorId
  , supervisorName       :: !SupervisorName
  , prevSupervisorStatus :: !SupervisorStatus
  , newSupervisorStatus  :: !SupervisorStatus
  , eventTime            :: !UTCTime
  }

data SupervisorSpec
  = SupervisorSpec {
    supervisorName      :: Text
  , supervisorIntensity :: !Int
  , notifyEvent         :: !(SupervisorEvent -> IO ())
  }

data ChildSpec
  = ChildSpec {
    childName   :: !ChildName
  , childAction :: !(IO ())
  }

data Child
  = Child {
    childId           :: !ChildId
  , childAsync        :: !(Async ())
  , childCreationTime :: !UTCTime
  , childSpec         :: !ChildSpec
  }

data ControlAction
  = ForkChild {
    childSpec         :: !ChildSpec
  , childRestartCount :: !RestartCount
  , returnChildId     :: !(ChildId -> IO ())
  }
  | TerminateChild {
    childId :: !ChildId
  }

newtype SupervisorException
  = TerminateChildException { childId :: ChildId }
  deriving (Show)

instance Exception SupervisorException

data MonitorEvent
  = ChildTerminated {
    childId           :: !ChildId
  , childName         :: !ChildName
  , childRestartCount :: !RestartCount
  , monitorEventTime  :: !UTCTime
  }
  | ChildFailed {
    childId           :: !ChildId
  , childName         :: !ChildName
  , childRestartCount :: !RestartCount
  , childError        :: !SomeException
  , monitorEventTime  :: !UTCTime
  }
  | ChildCompleted {
    childId          :: !ChildId
  , childName        :: !ChildName
  , monitorEventTime :: !UTCTime
  }
  deriving (Show)

data SupervisorStatus
  = Initializing
  | Running
  | Halting
  | Halted
  deriving (Eq)

data SupervisorMessage
  = ControlAction !ControlAction
  | MonitorEvent !MonitorEvent

data Supervisor
  = Supervisor {
    supervisorRuntime :: !SupervisorRuntime
  , supervisorEnv     :: !SupervisorEnv
  , supervisorAsync   :: !(Async ())
  }

data SupervisorRuntime
  = SupervisorRuntime {
    supervisorId        :: !SupervisorId
  , supervisorQueue     :: !(TQueue SupervisorMessage)
  , supervisorChildMap  :: !(IORef (HashMap ChildId Child))
  , supervisorStatusVar :: !(TVar SupervisorStatus)
  , supervisorSpec      :: !SupervisorSpec
  }

data SupervisorEnv
  = SupervisorEnv {
    supervisorId        :: !SupervisorId
  , supervisorName      :: !SupervisorName
  , supervisorQueue     :: !(TQueue SupervisorMessage)
  , supervisorChildMap  :: !(IORef (HashMap ChildId Child))
  , supervisorStatusVar :: !(TVar SupervisorStatus)
  , supervisorSpec      :: !SupervisorSpec
  , supervisorRuntime   :: !SupervisorRuntime
  , notifyEvent         :: !(SupervisorEvent -> IO ())
  }

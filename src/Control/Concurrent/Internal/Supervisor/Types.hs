{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Control.Concurrent.Internal.Supervisor.Types where

import Protolude

import Control.Teardown (ITeardown(..), Teardown)
import Data.Default                  (Default (..))
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM.TVar   (TVar)
import Data.HashMap.Strict           (HashMap)
import Data.IORef                    (IORef)
import Data.Time.Clock               (UTCTime)
import Data.UUID                     (UUID)

type SupervisorId = UUID
type ChildId = UUID
type ChildAction = IO ()
type ChildThreadId = ThreadId
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
  | SupervisedChildTerminated {
    supervisorName :: !SupervisorName
  , supervisorId   :: !SupervisorId
  , childThreadId  :: !ChildThreadId
  , childId        :: !ChildId
  , childName      :: !ChildName
  , terminationReason :: !Text
  , eventTime      :: !UTCTime
  }
  | SupervisedChildrenTerminationStarted {
    supervisorName :: !SupervisorName
  , supervisorId   :: !SupervisorId
  , terminationReason :: !Text
  , eventTime      :: !UTCTime
  }
  | SupervisedChildrenTerminationFinished {
    supervisorName :: !SupervisorName
  , supervisorId   :: !SupervisorId
  , terminationReason :: !Text
  , eventTime      :: !UTCTime
  }
  | SupervisorFailed {
    supervisorName :: !SupervisorName
  , supervisorId   :: !SupervisorId
  , supervisorError :: !SomeException
  , eventTime       :: !UTCTime
  }
  | SupervisorTerminated {
    supervisorName :: !SupervisorName
  , supervisorId   :: !SupervisorId
  , eventTime       :: !UTCTime
  }
  deriving (Generic, Show)

data ChildTerminationPolicy
  = BrutalTermination
  | TimeoutSeconds !Int
  deriving (Generic, Show, Eq, Ord)

instance Default ChildTerminationPolicy where
  def = TimeoutSeconds 3

instance NFData ChildTerminationPolicy

data SupervisorSpec
  = SupervisorSpec {
    supervisorName      :: Text
  -- , supervisorIntensity :: !Int
  , supervisorChildTerminationPolicy :: !ChildTerminationPolicy
  , notifyEvent         :: !(SupervisorEvent -> IO ())
  }

data ChildOptions
  = ChildOptions {
    childName :: !ChildName
  , childOnError      :: !(SomeException -> IO ())
  , childOnCompletion :: !(IO ())
  , childOnTermination :: !(IO ())
  }
  deriving (Generic)

data ChildSpec
  = ChildSpec {
    childName     :: !ChildName
  , childAction   :: !ChildAction
  , childOnError      :: !(SomeException -> IO ())
  , childOnCompletion :: !(IO ())
  , childOnTermination :: !(IO ())
  }
  deriving (Generic)

data Child
  = Child {
    childId           :: !ChildId
  , childAsync        :: !(Async ())
  , childCreationTime :: !UTCTime
  , childName         :: !ChildName
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
  , terminationReason :: !Text
  , notifyChildTermination :: !(IO ())
  }
  | TerminateSupervisor {
    notifySupervisorTermination :: !(IO ())
  }
  -- | TerminateChildren
  deriving (Generic)


data SupervisorException
  = TerminateChildException {
      childId           :: !ChildId
    , terminationReason :: !Text
    }
    deriving (Generic, Show)

instance Exception SupervisorException
instance NFData SupervisorException

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
  | Halted
  deriving (Generic, Show, Eq)

instance NFData SupervisorStatus

data SupervisorMessage
  = ControlAction !ControlAction
  | MonitorEvent !MonitorEvent
  deriving (Generic)

data Supervisor
  = Supervisor {
    supervisorRuntime :: !SupervisorRuntime
  , supervisorEnv     :: !SupervisorEnv
  , supervisorAsync   :: !(Async ())
  , supervisorTeardown :: !Teardown
  }

instance ITeardown Supervisor where
  teardown Supervisor {supervisorTeardown} =
    teardown supervisorTeardown

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

defSupervisorSpec :: SupervisorSpec
defSupervisorSpec =
  SupervisorSpec {
    supervisorName                        = "default-supervisor"

  -- One (1) restart every five (5) seconds
  -- , supervisorIntensity                   = 1
  -- , supervisorPeriodSeconds               = 5

  -- , supervisorRestartStrategy             = def
  , supervisorChildTerminationPolicy      = def
  , notifyEvent                           = const $ return ()
  }

defChildOptions :: ChildOptions
defChildOptions =
  ChildOptions {
    childName           = "default-child"
  , childOnError        = const $ return ()
  , childOnCompletion   = return ()
  , childOnTermination  = return ()
  }

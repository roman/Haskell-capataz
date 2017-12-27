{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Control.Concurrent.Internal.Supervisor.Types where

import Protolude

import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM.TVar   (TVar)
import Control.Teardown              (ITeardown (..), Teardown)
import Data.Default                  (Default (..))
import Data.HashMap.Strict           (HashMap)
import Data.IORef                    (IORef)
import Data.Time.Clock               (NominalDiffTime, UTCTime)
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
    supervisorName    :: !SupervisorName
  , supervisorId      :: !SupervisorId
  , childThreadId     :: !ChildThreadId
  , childId           :: !ChildId
  , childName         :: !ChildName
  , terminationReason :: !Text
  , eventTime         :: !UTCTime
  }
  | SupervisedChildStarted {
    supervisorName :: !SupervisorName
  , supervisorId   :: !SupervisorId
  , childThreadId  :: !ChildThreadId
  , childId        :: !ChildId
  , childName      :: !ChildName
  , eventTime      :: !UTCTime
  }
  | SupervisedChildRestarted {
    supervisorName    :: !SupervisorName
  , supervisorId      :: !SupervisorId
  , childThreadId     :: !ChildThreadId
  , childId           :: !ChildId
  , childName         :: !ChildName
  , childRestartCount :: !Int
  , eventTime         :: !UTCTime
  }
  | SupervisedChildCompleted {
    supervisorName :: !SupervisorName
  , supervisorId   :: !SupervisorId
  , childThreadId  :: !ChildThreadId
  , childId        :: !ChildId
  , childName      :: !ChildName
  , eventTime      :: !UTCTime
  }
  | SupervisedChildFailed {
    supervisorName :: !SupervisorName
  , supervisorId   :: !SupervisorId
  , childThreadId  :: !ChildThreadId
  , childId        :: !ChildId
  , childName      :: !ChildName
  , childError     :: !SomeException
  , eventTime      :: !UTCTime
  }
  | SupervisedChildCallbackExecuted {
    supervisorName     :: !SupervisorName
  , supervisorId       :: !SupervisorId
  , childThreadId      :: !ChildThreadId
  , childId            :: !ChildId
  , childName          :: !ChildName
  , childCallbackError :: !(Maybe SomeException)
  , callbackType       :: !CallbackType
  , eventTime          :: !UTCTime
  }
  | SupervisedChildrenTerminationStarted {
    supervisorName    :: !SupervisorName
  , supervisorId      :: !SupervisorId
  , terminationReason :: !Text
  , eventTime         :: !UTCTime
  }
  | SupervisedChildrenTerminationFinished {
    supervisorName    :: !SupervisorName
  , supervisorId      :: !SupervisorId
  , terminationReason :: !Text
  , eventTime         :: !UTCTime
  }
  | SupervisorFailed {
    supervisorName  :: !SupervisorName
  , supervisorId    :: !SupervisorId
  , supervisorError :: !SomeException
  , eventTime       :: !UTCTime
  }
  | SupervisorTerminated {
    supervisorName :: !SupervisorName
  , supervisorId   :: !SupervisorId
  , eventTime      :: !UTCTime
  }
  | SupervisorShutdownInvoked {
    supervisorName :: !SupervisorName
  , supervisorId   :: !SupervisorId
  , eventTime      :: !UTCTime
  }
  deriving (Generic, Show)

data ChildTerminationPolicy
  = Infinity
  | BrutalTermination
  | TimeoutMillis !Int
  deriving (Generic, Show, Eq, Ord)

instance Default ChildTerminationPolicy where
  def = TimeoutMillis 3000

instance NFData ChildTerminationPolicy

data ChildRestartAction
  = ResetRestartCount
  | IncreaseRestartCount
  | HaltSupervisor
  deriving (Generic, Show, Eq)

instance NFData ChildRestartAction

data ChildTerminationOrder
  = NewestFirst -- ^ Terminate child threads from most recent to oldest
  | OldestFirst -- ^ Terminate child threads from oldest to most recent
  deriving (Generic, Show, Eq, Ord)

instance Default ChildTerminationOrder where
  def = OldestFirst

instance NFData ChildTerminationOrder

data SupervisorRestartStrategy
  = AllForOne
    -- ^ Terminate all children threads when one fails and restart them all
  | OneForOne
    -- ^ Only restart child thread that terminated
  deriving (Generic, Show, Eq, Ord)

instance Default SupervisorRestartStrategy where
  def = OneForOne

instance NFData SupervisorRestartStrategy

data SupervisorOptions
  = SupervisorOptions {
    supervisorName                  :: Text
  , supervisorIntensity             :: !Int
    -- ^ http://erlang.org/doc/design_principles/sup_princ.html#max_intensity
  , supervisorPeriodSeconds         :: !NominalDiffTime
  , supervisorRestartStrategy       :: !SupervisorRestartStrategy
  , supervisorChildSpecList         :: ![ChildSpec]
  , supervisorChildTerminationOrder :: !ChildTerminationOrder
  , onSupervisorIntensityReached    :: !(IO ())
  , notifyEvent                     :: !(SupervisorEvent -> IO ())
  }

data ChildOptions
  = ChildOptions {
    childName              :: !ChildName
  , childOnFailure         :: !(SomeException -> IO ())
  , childOnCompletion      :: !(IO ())
  , childOnTermination     :: !(IO ())
  , childTerminationPolicy :: !ChildTerminationPolicy
  , childRestartStrategy   :: !ChildRestartStrategy
  }
  deriving (Generic)

data ChildRestartStrategy
  = Permanent
  -- ^ Child thread is always restarted on completion
  | Transient
  -- ^ Child thread is restarted only if completed with failure
  | Temporary
  -- ^ Child thread is never restarted on completion
  deriving (Generic, Show, Eq)

instance NFData ChildRestartStrategy
instance Default ChildRestartStrategy where
  def = Permanent

data ChildSpec
  = ChildSpec {
    childAction            :: ChildAction
    -- ^ ChildAction is lazy by default because we want to eval
    -- in on a child thread, not on the supervisor thread
  , childName              :: !ChildName
  , childOnFailure         :: !(SomeException -> IO ())
  , childOnCompletion      :: !(IO ())
  , childOnTermination     :: !(IO ())
  , childTerminationPolicy :: !ChildTerminationPolicy
  , childRestartStrategy   :: !ChildRestartStrategy
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

data ChildEnv
  = ChildEnv {
    childAction          :: ChildAction
    -- ^ ChildAction is lazy by default because we want to eval
    -- in on a child thread, not on the supervisor thread
  , childId              :: !ChildId
  , childAsync           :: !(Async ())
  , childCreationTime    :: !UTCTime
  , childName            :: !ChildName
  , childSpec            :: !ChildSpec
  , childOnFailure       :: !(SomeException -> IO ())
  , childOnCompletion    :: !(IO ())
  , childOnTermination   :: !(IO ())
  , childRestartStrategy :: !ChildRestartStrategy
  }

data ControlAction
  = ForkChild {
    childSpec     :: !ChildSpec
  , returnChildId :: !(ChildId -> IO ())
  }
  | TerminateChild {
    childId                :: !ChildId
  , terminationReason      :: !Text
  , notifyChildTermination :: !(IO ())
  }
  | TerminateSupervisor {
    notifySupervisorTermination :: !(IO ())
  }
  -- | TerminateChildren
  deriving (Generic)


data SupervisorSignal
  = RestartChildException
  | TerminateChildException {
      childId                :: !ChildId
    , childTerminationReason :: !Text
    }
  | BrutallyTerminateChildException {
      childId                :: !ChildId
    , childTerminationReason :: !Text
    }
    deriving (Generic, Show)

instance Exception SupervisorSignal
instance NFData SupervisorSignal

data SupervisorError
  = SupervisorIntensityReached {
    childId           :: !ChildId
  , childName         :: !ChildName
  , childRestartCount :: !Int
  }
  deriving (Generic, Show)

instance Exception SupervisorError
instance NFData SupervisorError

data CallbackType
  = OnCompletion
  | OnFailure
  | OnTermination
  deriving (Generic, Show, Eq)

data ChildError
  = ChildCallbackFailed {
      childId            :: !ChildId
    , childActionError   :: !(Maybe SomeException)
    , callbackType       :: !CallbackType
    , childCallbackError :: !SomeException
    }
    deriving (Generic, Show)

instance Exception ChildError

data MonitorEvent
  = ChildTerminated {
    childId                :: !ChildId
  , childName              :: !ChildName
  , childRestartCount      :: !RestartCount
  , childTerminationReason :: !Text
  , monitorEventTime       :: !UTCTime
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
  | ChildForcedRestart {
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
  deriving (Generic, Show, Eq)

instance NFData SupervisorStatus

data SupervisorMessage
  = ControlAction !ControlAction
  | MonitorEvent !MonitorEvent
  deriving (Generic)

data Supervisor
  = Supervisor {
    supervisorRuntime  :: !SupervisorRuntime
  , supervisorEnv      :: !SupervisorEnv
  , supervisorAsync    :: !(Async ())
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
  , supervisorOptions   :: !SupervisorOptions
  }

data SupervisorEnv
  = SupervisorEnv {
    supervisorId                    :: !SupervisorId
  , supervisorName                  :: !SupervisorName
  , supervisorQueue                 :: !(TQueue SupervisorMessage)
  , supervisorChildMap              :: !(IORef (HashMap ChildId Child))
  , supervisorStatusVar             :: !(TVar SupervisorStatus)
  , supervisorOptions               :: !SupervisorOptions
  , supervisorRuntime               :: !SupervisorRuntime
  , supervisorIntensity             :: !Int
    -- ^ http://erlang.org/doc/design_principles/sup_princ.html#max_intensity
  , supervisorPeriodSeconds         :: !NominalDiffTime
  , supervisorRestartStrategy       :: !SupervisorRestartStrategy
  , supervisorChildTerminationOrder :: !ChildTerminationOrder
  , onSupervisorIntensityReached    :: !(IO ())
  , notifyEvent                     :: !(SupervisorEvent -> IO ())
  }

defSupervisorOptions :: SupervisorOptions
defSupervisorOptions = SupervisorOptions
  { supervisorName                  = "default-supervisor"

  -- One (1) restart every five (5) seconds
  , supervisorIntensity             = 1
  , supervisorPeriodSeconds         = 5
  , supervisorRestartStrategy       = def
  , supervisorChildSpecList         = []
  , supervisorChildTerminationOrder = OldestFirst
  , onSupervisorIntensityReached    = return ()
  , notifyEvent                     = const $ return ()
  }

defChildOptions :: ChildOptions
defChildOptions = ChildOptions
  { childName              = "default-child"
  , childOnFailure         = const $ return ()
  , childOnCompletion      = return ()
  , childOnTermination     = return ()
  , childTerminationPolicy = def
  , childRestartStrategy   = def
  }

defChildSpec :: ChildSpec
defChildSpec = ChildSpec
  { childName              = "default-child"
  , childAction            = return ()
  , childOnFailure         = const $ return ()
  , childOnCompletion      = return ()
  , childOnTermination     = return ()
  , childTerminationPolicy = def
  , childRestartStrategy   = def
  }

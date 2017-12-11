{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Control.Concurrent.Supervisor.Internal.Types where

import Protolude

import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM.TVar   (TVar)
import Data.Default                  (Default (..))
import Data.HashMap.Strict           (HashMap)
import Data.IORef                    (IORef)
import Data.Time.Clock               (NominalDiffTime, UTCTime)
import Data.UUID                     (UUID)

type RestartCount = Int
type ChildThreadId = ThreadId
type ChildName = Text
type SupervisorName = Text
type ChildId = UUID
type SupervisorId = UUID
type ChildMap = HashMap ChildId Child

data ChildTerminationOrder
  = NewestFirst -- ^ Terminate child threads from most recent to oldest
  | OldestFirst -- ^ Terminate child threads from oldest to most recent
  deriving (Generic, Show, Eq, Ord)

instance Default ChildTerminationOrder where
  def = OldestFirst

instance NFData ChildTerminationOrder

data ChildTerminationPolicy
  = BrutalTermination
  | TimeoutSeconds !Int
  deriving (Generic, Show, Eq, Ord)

instance Default ChildTerminationPolicy where
  def = TimeoutSeconds 3

instance NFData ChildTerminationPolicy

data SupervisorRestartStrategy
  = AllForOne !ChildTerminationOrder -- ^ Terminate all children threads when one fails and restart them all
  | OneForOne -- ^ Only restart child thread that terminated
  deriving (Generic, Show, Eq, Ord)

instance Default SupervisorRestartStrategy where
  def = OneForOne

instance NFData SupervisorRestartStrategy

data SupervisorRuntime
  = SupervisorRuntime {
    supervisorId         :: !SupervisorId
  , supervisorChildMap   :: !(IORef ChildMap)
  , supervisorEventQueue :: !(TQueue MonitorEvent )
  , supervisorStatusVar  :: !(TVar SupervisorStatus)
  , supervisorSpec       :: !SupervisorSpec
  }

data Supervisor
  = Supervisor {
    supervisorRuntime :: !SupervisorRuntime
  , supervisorAsync   :: !(Async ())
  }

data SupervisorSpec
  = SupervisorSpec {
    supervisorName                        :: !SupervisorName
  , supervisorIntensity                   :: !Int
    -- ^ http://erlang.org/doc/design_principles/sup_princ.html#max_intensity
  , supervisorPeriodSeconds               :: !NominalDiffTime
  , supervisorRestartStrategy             :: !SupervisorRestartStrategy
  , supervisorChildTerminationPolicy :: !ChildTerminationPolicy
  , supervisorShutdownTimeoutSeconds      :: !Int
  , notifyEvent                           :: !(SupervisorEvent -> IO ())
  }

instance Default SupervisorSpec where
  def = defSupervisorSpec

data SupervisorEnv
  = SupervisorEnv {
    supervisorId                          :: !SupervisorId
  , supervisorName                        :: !SupervisorName
  , supervisorChildMap                    :: !(IORef ChildMap)
  , supervisorEventQueue                  :: !(TQueue MonitorEvent )
  , supervisorStatusVar                   :: !(TVar SupervisorStatus)
  , supervisorIntensity                   :: !Int
  , supervisorRestartStrategy             :: !SupervisorRestartStrategy
  , supervisorChildShutdownTimeoutSeconds :: !Int
  , supervisorShutdownTimeoutSeconds      :: !Int
  , supervisorPeriodSeconds               :: !NominalDiffTime
  , notifyEvent                           :: !(SupervisorEvent -> IO ())
  }

data SupervisorStatus
  = Initializing
  | Running
  | Halting
  | Halted
  deriving (Generic, Show, Eq)

instance NFData SupervisorStatus

data Child
  = Child {
    childId           :: !ChildId
  , childAsync        :: !(Async ())
  , childCreationTime :: !UTCTime
  , childSpec         :: !ChildSpec
  }

data ChildRestartAction
  = ResetRestartCount
  | IncreaseRestartCount
  | HaltSupervisor
  deriving (Generic, Show, Eq)

instance NFData ChildRestartAction

data ChildSpec
  = ChildSpec {
    childName            :: !ChildName
  , childAction          :: !(IO ())
  , childOnError         :: !(SomeException -> IO ())
  , childOnFinished      :: !(IO ())
  , childRestartStrategy :: !ChildRestartStrategy
  }

data ChildRestartStrategy
  = Permanent -- ^ Child thread is always restarted on completion
  | Transient -- ^ Child thread is restarted only if completed with failure
  | Temporal  -- ^ Child thread is never restarted on completion
  deriving (Generic, Show, Eq)

instance NFData ChildRestartStrategy

data ChildEnv
  = ChildEnv {
    childId                               :: !ChildId
  , childAsync                            :: !(Async ())
  , childCreationTime                     :: !UTCTime
  , childSpec                             :: !ChildSpec
  , childName                             :: !ChildName
  , childRestartStrategy                  :: !ChildRestartStrategy
  , supervisorEnv                         :: !SupervisorEnv
  , supervisorId                          :: !SupervisorId
  , supervisorStatusVar                   :: !(TVar SupervisorStatus)
  , supervisorName                        :: !SupervisorName
  , supervisorIntensity                   :: !Int
  , supervisorPeriodSeconds               :: !NominalDiffTime
  , supervisorRestartStrategy             :: !SupervisorRestartStrategy
  , supervisorChildShutdownTimeoutSeconds :: !Int
  , supervisorShutdownTimeoutSeconds      :: !Int
  , notifyEvent                           :: !(SupervisorEvent -> IO ())
  }

data MonitorEvent
  -- ^ child thread finished with errors
  = Failed {
    childName         :: !ChildName
  , childId           :: !ChildId
  , childRestartCount :: !RestartCount
  , eventTime         :: !UTCTime
  , eventError        :: !SomeException
  }
  -- ^ child thread finished without any errors
  | Finished {
    childName :: !ChildName
  , childId   :: !ChildId
  , eventTime :: !UTCTime
  }
  -- ^ child thread was finished from supervisor
  | Terminated {
    childName         :: !ChildName
  , childId           :: !ChildId
  , childRestartCount :: !RestartCount
  , eventTime         :: !UTCTime
  }

data ChildException
  = TerminateChild {
      childId           :: !ChildId
    , terminationReason :: !Text
    }
    deriving (Generic, Show)

instance Exception ChildException
instance NFData ChildException

--------------------------------------------------------------------------------

data SupervisorEvent
  = SupervisorStateError
    { supervisorName :: !SupervisorName
    , supervisorId   :: !SupervisorId
    , errorMessage   :: !Text
    , eventTime      :: !UTCTime }
  | SupervisorStarted
    { supervisorName :: !SupervisorName
    , supervisorId   :: !SupervisorId
    , eventTime      :: !UTCTime }
  | SupervisorHalted
    { supervisorName :: !SupervisorName
    , supervisorId   :: !SupervisorId
    , eventTime      :: !UTCTime }
  | ChildAlreadyHalted
    { supervisorName :: !SupervisorName
    , supervisorId   :: !SupervisorId
    , childId        :: !ChildId
    , eventTime      :: !UTCTime }
  | ChildStarted
    { supervisorName :: !SupervisorName
    , supervisorId   :: !SupervisorId
    , childThreadId  :: !ChildThreadId
    , childId        :: !ChildId
    , childName      :: !ChildName
    , eventTime      :: !UTCTime }
  | ChildRestartCountReset
    { supervisorName :: !SupervisorName
    , supervisorId   :: !SupervisorId
    , childId        :: !ChildId
    , childThreadId  :: !ChildThreadId
    , childName      :: !ChildName
    , eventTime      :: !UTCTime }
  | ChildRestarted
    { supervisorName     :: !SupervisorName
    , supervisorId       :: !SupervisorId
    , childId            :: !ChildId
    , childName          :: !ChildName
    , eventRestartReason :: !Text
    , eventTime          :: !UTCTime }
  | ChildTerminated
    { supervisorName :: !SupervisorName
    , supervisorId   :: !SupervisorId
    , childThreadId  :: !ChildThreadId
    , childId        :: !ChildId
    , childName      :: !ChildName
    , eventTime      :: !UTCTime }
  | ChildDropped
    { supervisorName :: !SupervisorName
    , supervisorId   :: !SupervisorId
    , childThreadId  :: !ChildThreadId
    , childId        :: !ChildId
    , childName      :: !ChildName
    , eventTime      :: !UTCTime }

defSupervisorSpec :: SupervisorSpec
defSupervisorSpec =
  SupervisorSpec {
    supervisorName                        = "default-supervisor"

  -- One (1) restart every five (5) seconds
  , supervisorIntensity                   = 1
  , supervisorPeriodSeconds               = 5

  , supervisorRestartStrategy             = def
  , supervisorChildTerminationPolicy      = def
  , supervisorShutdownTimeoutSeconds      = 10
  , notifyEvent                           = const $ return ()
  }

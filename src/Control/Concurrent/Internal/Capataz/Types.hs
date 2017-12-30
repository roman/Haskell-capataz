{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Control.Concurrent.Internal.Capataz.Types where

import Protolude

import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM.TVar   (TVar)
import Control.Teardown              (ITeardown (..), Teardown)
import Data.Default                  (Default (..))
import Data.HashMap.Strict           (HashMap)
import Data.IORef                    (IORef)
import Data.Time.Clock               (NominalDiffTime, UTCTime)
import Data.UUID                     (UUID)

type CapatazId = UUID
type WorkerId = UUID
type WorkerAction = IO ()
type WorkerThreadId = ThreadId
type CapatazName = Text
type WorkerName = Text
type RestartCount = Int
type WorkerMap = HashMap WorkerId Worker

data CapatazEvent
  = InvalidCapatazStatusReached {
    capatazId   :: !CapatazId
  , capatazName :: !CapatazName
  , eventTime      :: !UTCTime
  }
  | CapatazStatusChanged {
    capatazId         :: !CapatazId
  , capatazName       :: !CapatazName
  , prevCapatazStatus :: !CapatazStatus
  , newCapatazStatus  :: !CapatazStatus
  , eventTime            :: !UTCTime
  }
  | SupervisedWorkerTerminated {
    capatazName    :: !CapatazName
  , capatazId      :: !CapatazId
  , workerThreadId     :: !WorkerThreadId
  , workerId           :: !WorkerId
  , workerName         :: !WorkerName
  , terminationReason :: !Text
  , eventTime         :: !UTCTime
  }
  | SupervisedWorkerStarted {
    capatazName :: !CapatazName
  , capatazId   :: !CapatazId
  , workerThreadId  :: !WorkerThreadId
  , workerId        :: !WorkerId
  , workerName      :: !WorkerName
  , eventTime      :: !UTCTime
  }
  | SupervisedWorkerRestarted {
    capatazName    :: !CapatazName
  , capatazId      :: !CapatazId
  , workerThreadId     :: !WorkerThreadId
  , workerId           :: !WorkerId
  , workerName         :: !WorkerName
  , workerRestartCount :: !Int
  , eventTime         :: !UTCTime
  }
  | SupervisedWorkerCompleted {
    capatazName :: !CapatazName
  , capatazId   :: !CapatazId
  , workerThreadId  :: !WorkerThreadId
  , workerId        :: !WorkerId
  , workerName      :: !WorkerName
  , eventTime      :: !UTCTime
  }
  | SupervisedWorkerFailed {
    capatazName :: !CapatazName
  , capatazId   :: !CapatazId
  , workerThreadId  :: !WorkerThreadId
  , workerId        :: !WorkerId
  , workerName      :: !WorkerName
  , workerError     :: !SomeException
  , eventTime      :: !UTCTime
  }
  | SupervisedWorkerCallbackExecuted {
    capatazName     :: !CapatazName
  , capatazId       :: !CapatazId
  , workerThreadId      :: !WorkerThreadId
  , workerId            :: !WorkerId
  , workerName          :: !WorkerName
  , workerCallbackError :: !(Maybe SomeException)
  , callbackType       :: !CallbackType
  , eventTime          :: !UTCTime
  }
  | SupervisedWorkersTerminationStarted {
    capatazName    :: !CapatazName
  , capatazId      :: !CapatazId
  , terminationReason :: !Text
  , eventTime         :: !UTCTime
  }
  | SupervisedWorkersTerminationFinished {
    capatazName    :: !CapatazName
  , capatazId      :: !CapatazId
  , terminationReason :: !Text
  , eventTime         :: !UTCTime
  }
  | CapatazFailed {
    capatazName  :: !CapatazName
  , capatazId    :: !CapatazId
  , capatazError :: !SomeException
  , eventTime       :: !UTCTime
  }
  | CapatazTerminated {
    capatazName :: !CapatazName
  , capatazId   :: !CapatazId
  , eventTime      :: !UTCTime
  }
  | CapatazShutdownInvoked {
    capatazName :: !CapatazName
  , capatazId   :: !CapatazId
  , eventTime      :: !UTCTime
  }
  deriving (Generic, Show)

data WorkerTerminationPolicy
  = Infinity
  | BrutalTermination
  | TimeoutMillis !Int
  deriving (Generic, Show, Eq, Ord)

instance Default WorkerTerminationPolicy where
  def = TimeoutMillis 3000

instance NFData WorkerTerminationPolicy

data WorkerRestartAction
  = ResetRestartCount
  | IncreaseRestartCount
  | HaltCapataz
  deriving (Generic, Show, Eq)

instance NFData WorkerRestartAction

data WorkerTerminationOrder
  = NewestFirst -- ^ Terminate worker threads from most recent to oldest
  | OldestFirst -- ^ Terminate worker threads from oldest to most recent
  deriving (Generic, Show, Eq, Ord)

instance Default WorkerTerminationOrder where
  def = OldestFirst

instance NFData WorkerTerminationOrder

data CapatazRestartStrategy
  = AllForOne
    -- ^ Terminate all workers threads when one fails and restart them all
  | OneForOne
    -- ^ Only restart worker thread that terminated
  deriving (Generic, Show, Eq, Ord)

instance Default CapatazRestartStrategy where
  def = OneForOne

instance NFData CapatazRestartStrategy

data CapatazOptions
  = CapatazOptions {
    capatazName                  :: Text
  , capatazIntensity             :: !Int
    -- ^ http://erlang.org/doc/design_principles/sup_princ.html#max_intensity
  , capatazPeriodSeconds         :: !NominalDiffTime
  , capatazRestartStrategy       :: !CapatazRestartStrategy
  , capatazWorkerSpecList         :: ![WorkerSpec]
  , capatazWorkerTerminationOrder :: !WorkerTerminationOrder
  , onCapatazIntensityReached    :: !(IO ())
  , notifyEvent                     :: !(CapatazEvent -> IO ())
  }

data WorkerOptions
  = WorkerOptions {
    workerName              :: !WorkerName
  , workerOnFailure         :: !(SomeException -> IO ())
  , workerOnCompletion      :: !(IO ())
  , workerOnTermination     :: !(IO ())
  , workerTerminationPolicy :: !WorkerTerminationPolicy
  , workerRestartStrategy   :: !WorkerRestartStrategy
  }
  deriving (Generic)

data WorkerRestartStrategy
  = Permanent
  -- ^ Worker thread is always restarted on completion
  | Transient
  -- ^ Worker thread is restarted only if completed with failure
  | Temporary
  -- ^ Worker thread is never restarted on completion
  deriving (Generic, Show, Eq)

instance NFData WorkerRestartStrategy
instance Default WorkerRestartStrategy where
  def = Permanent

data WorkerSpec
  = WorkerSpec {
    workerAction            :: WorkerAction
    -- ^ WorkerAction is lazy by default because we want to eval
    -- in on a worker thread, not on the capataz thread
  , workerName              :: !WorkerName
  , workerOnFailure         :: !(SomeException -> IO ())
  , workerOnCompletion      :: !(IO ())
  , workerOnTermination     :: !(IO ())
  , workerTerminationPolicy :: !WorkerTerminationPolicy
  , workerRestartStrategy   :: !WorkerRestartStrategy
  }
  deriving (Generic)

data Worker
  = Worker {
    workerId           :: !WorkerId
  , workerAsync        :: !(Async ())
  , workerCreationTime :: !UTCTime
  , workerName         :: !WorkerName
  , workerSpec         :: !WorkerSpec
  }

data WorkerEnv
  = WorkerEnv {
    workerAction          :: WorkerAction
    -- ^ WorkerAction is lazy by default because we want to eval
    -- in on a worker thread, not on the capataz thread
  , workerId              :: !WorkerId
  , workerAsync           :: !(Async ())
  , workerCreationTime    :: !UTCTime
  , workerName            :: !WorkerName
  , workerSpec            :: !WorkerSpec
  , workerOnFailure       :: !(SomeException -> IO ())
  , workerOnCompletion    :: !(IO ())
  , workerOnTermination   :: !(IO ())
  , workerRestartStrategy :: !WorkerRestartStrategy
  }

data ControlAction
  = ForkWorker {
    workerSpec     :: !WorkerSpec
  , returnWorkerId :: !(WorkerId -> IO ())
  }
  | TerminateWorker {
    workerId                :: !WorkerId
  , terminationReason      :: !Text
  , notifyWorkerTermination :: !(IO ())
  }
  | TerminateCapataz {
    notifyCapatazTermination :: !(IO ())
  }
  deriving (Generic)


data CapatazSignal
  = RestartWorkerException
  | TerminateWorkerException {
      workerId                :: !WorkerId
    , workerTerminationReason :: !Text
    }
  | BrutallyTerminateWorkerException {
      workerId                :: !WorkerId
    , workerTerminationReason :: !Text
    }
    deriving (Generic, Show)

instance Exception CapatazSignal
instance NFData CapatazSignal

data CapatazError
  = CapatazIntensityReached {
    workerId           :: !WorkerId
  , workerName         :: !WorkerName
  , workerRestartCount :: !Int
  }
  deriving (Generic, Show)

instance Exception CapatazError
instance NFData CapatazError

data CallbackType
  = OnCompletion
  | OnFailure
  | OnTermination
  deriving (Generic, Show, Eq)

data WorkerError
  = WorkerCallbackFailed {
      workerId            :: !WorkerId
    , workerActionError   :: !(Maybe SomeException)
    , callbackType       :: !CallbackType
    , workerCallbackError :: !SomeException
    }
    deriving (Generic, Show)

instance Exception WorkerError

data MonitorEvent
  = WorkerTerminated {
    workerId                :: !WorkerId
  , workerName              :: !WorkerName
  , workerRestartCount      :: !RestartCount
  , workerTerminationReason :: !Text
  , monitorEventTime       :: !UTCTime
  }
  | WorkerFailed {
    workerId           :: !WorkerId
  , workerName         :: !WorkerName
  , workerRestartCount :: !RestartCount
  , workerError        :: !SomeException
  , monitorEventTime  :: !UTCTime
  }
  | WorkerCompleted {
    workerId          :: !WorkerId
  , workerName        :: !WorkerName
  , monitorEventTime :: !UTCTime
  }
  | WorkerForcedRestart {
    workerId          :: !WorkerId
  , workerName        :: !WorkerName
  , monitorEventTime :: !UTCTime
  }
  deriving (Show)

data CapatazStatus
  = Initializing
  | Running
  | Halting
  | Halted
  deriving (Generic, Show, Eq)

instance NFData CapatazStatus

data CapatazMessage
  = ControlAction !ControlAction
  | MonitorEvent !MonitorEvent
  deriving (Generic)

data Capataz
  = Capataz {
    capatazRuntime  :: !CapatazRuntime
  , capatazEnv      :: !CapatazEnv
  , capatazAsync    :: !(Async ())
  , capatazTeardown :: !Teardown
  }

instance ITeardown Capataz where
  teardown Capataz {capatazTeardown} =
    teardown capatazTeardown

data CapatazRuntime
  = CapatazRuntime {
    capatazId        :: !CapatazId
  , capatazQueue     :: !(TQueue CapatazMessage)
  , capatazWorkerMap  :: !(IORef (HashMap WorkerId Worker))
  , capatazStatusVar :: !(TVar CapatazStatus)
  , capatazOptions   :: !CapatazOptions
  }

data CapatazEnv
  = CapatazEnv {
    capatazId                    :: !CapatazId
  , capatazName                  :: !CapatazName
  , capatazQueue                 :: !(TQueue CapatazMessage)
  , capatazWorkerMap              :: !(IORef (HashMap WorkerId Worker))
  , capatazStatusVar             :: !(TVar CapatazStatus)
  , capatazOptions               :: !CapatazOptions
  , capatazRuntime               :: !CapatazRuntime
  , capatazIntensity             :: !Int
    -- ^ http://erlang.org/doc/design_principles/sup_princ.html#max_intensity
  , capatazPeriodSeconds         :: !NominalDiffTime
  , capatazRestartStrategy       :: !CapatazRestartStrategy
  , capatazWorkerTerminationOrder :: !WorkerTerminationOrder
  , onCapatazIntensityReached    :: !(IO ())
  , notifyEvent                     :: !(CapatazEvent -> IO ())
  }

defCapatazOptions :: CapatazOptions
defCapatazOptions = CapatazOptions
  { capatazName                  = "default-capataz"

  -- One (1) restart every five (5) seconds
  , capatazIntensity             = 1
  , capatazPeriodSeconds         = 5
  , capatazRestartStrategy       = def
  , capatazWorkerSpecList         = []
  , capatazWorkerTerminationOrder = OldestFirst
  , onCapatazIntensityReached    = return ()
  , notifyEvent                     = const $ return ()
  }

defWorkerOptions :: WorkerOptions
defWorkerOptions = WorkerOptions
  { workerName              = "default-worker"
  , workerOnFailure         = const $ return ()
  , workerOnCompletion      = return ()
  , workerOnTermination     = return ()
  , workerTerminationPolicy = def
  , workerRestartStrategy   = def
  }

defWorkerSpec :: WorkerSpec
defWorkerSpec = WorkerSpec
  { workerName              = "default-worker"
  , workerAction            = return ()
  , workerOnFailure         = const $ return ()
  , workerOnCompletion      = return ()
  , workerOnTermination     = return ()
  , workerTerminationPolicy = def
  , workerRestartStrategy   = def
  }

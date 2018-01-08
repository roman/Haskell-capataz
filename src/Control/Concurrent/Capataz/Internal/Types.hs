{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-| This module contains all the types used across all the other modules -}
module Control.Concurrent.Capataz.Internal.Types where

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
type SupervisorId = UUID
type SupervisorStatus = CapatazStatus
type ProcessId = UUID
type WorkerAction = IO ()
type ProcessThreadId = ThreadId
type ProcessName = Text
type CapatazName = Text
type SupervisorName = Text
type WorkerName = Text
type RestartCount = Int
type ProcessMap = HashMap ProcessId Process

-- | Event passed to the "notifyEvent" callback sub-routine, this events can be
-- used to monitor the capataz system and understanding what is doing. This
-- provides high levels of telemetry for the Capataz instance, so is mainly used
-- for logging, monitoring and testing purposes.
data CapatazEvent
  = InvalidCapatazStatusReached {
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
  | ProcessTerminated {
    supervisorId      :: !SupervisorId
  , supervisorName    :: !SupervisorName
  , processThreadId   :: !ProcessThreadId
  , processId         :: !ProcessId
  , processName       :: !ProcessName
  , terminationReason :: !Text
  , eventTime         :: !UTCTime
  }
  | ProcessStarted {
    supervisorId    :: !SupervisorId
  , supervisorName  :: !SupervisorName
  , processThreadId :: !ProcessThreadId
  , processId       :: !ProcessId
  , processName     :: !ProcessName
  , eventTime       :: !UTCTime
  }
  | ProcessRestarted {
    supervisorId        :: !SupervisorId
  , supervisorName      :: !SupervisorName
  , processThreadId     :: !ProcessThreadId
  , processId           :: !ProcessId
  , processName         :: !ProcessName
  , processRestartCount :: !Int
  , eventTime           :: !UTCTime
  }
  | ProcessCompleted {
    supervisorId    :: !SupervisorId
  , supervisorName  :: !SupervisorName
  , processThreadId :: !ProcessThreadId
  , processId       :: !ProcessId
  , processName     :: !ProcessName
  , eventTime       :: !UTCTime
  }
  | ProcessFailed {
    supervisorName  :: !SupervisorName
  , supervisorId    :: !SupervisorId
  , processThreadId :: !ProcessThreadId
  , processId       :: !ProcessId
  , processName     :: !ProcessName
  , processError    :: !SomeException
  , eventTime       :: !UTCTime
  }
  | ProcessCallbackExecuted {
    supervisorId         :: !SupervisorId
  , supervisorName       :: !SupervisorName
  , processThreadId      :: !ProcessThreadId
  , processId            :: !ProcessId
  , processName          :: !ProcessName
  , processCallbackError :: !(Maybe SomeException)
  , processCallbackType  :: !CallbackType
  , eventTime            :: !UTCTime
  }
  | ProcessTerminationStarted {
    supervisorName    :: !SupervisorName
  , supervisorId      :: !SupervisorId
  , terminationReason :: !Text
  , eventTime         :: !UTCTime
  }
  | ProcessTerminationFinished {
    supervisorName    :: !SupervisorName
  , supervisorId      :: !SupervisorId
  , terminationReason :: !Text
  , eventTime         :: !UTCTime
  }
  | SupervisorTerminated {
    supervisorName :: !SupervisorName
  , supervisorId   :: !SupervisorId
  , eventTime      :: !UTCTime
  }
  | CapatazFailed {
    supervisorId    :: !SupervisorId
  , supervisorName  :: !SupervisorName
  , supervisorError :: !SomeException
  , eventTime       :: !UTCTime
  }
  | CapatazTerminated {
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

-- | Defines how a "Worker" termination should be handled, default
-- "WorkerTerminationPolicy" is 3 seconds
data WorkerTerminationPolicy
  -- | Waits until infinity for the worker to terminate
  = Infinity

  -- | Worker is terminated wihtout a chance to call its callback
  | BrutalTermination

  -- | Allows n milliseconds for worker termination callback to be
  -- executed, otherwise "BrutalTermination occurs"
  | TimeoutMillis !Int
  deriving (Generic, Show, Eq, Ord)

instance Default WorkerTerminationPolicy where
  def = TimeoutMillis 3000

instance NFData WorkerTerminationPolicy

-- | Helper record to assess if the capataz error intensity has been breached
data WorkerRestartAction
  -- | The capataz will restart the failed worker and reset the restart count
  -- given intensity period has passed
  = ResetRestartCount

  -- | The capataz will restart the failed worker and increase the restart count
  | IncreaseRestartCount

  -- | The error intensity has been reached
  | HaltCapataz
  deriving (Generic, Show, Eq)

instance NFData WorkerRestartAction

-- | Specifies how order in which process should be terminated by a Capataz in
-- case of restart or shutdown; default is "OldestFirst"
data ProcessTerminationOrder
  -- | Terminate process threads from most recent to oldest
  = NewestFirst
  -- | Terminate process threads from oldest to most recent
  | OldestFirst
  deriving (Generic, Show, Eq, Ord)

instance Default ProcessTerminationOrder where
  def = OldestFirst

instance NFData ProcessTerminationOrder

-- | Specifies how a Capataz should restart a failing worker. Default is
-- "OneForOne"
data SupervisorRestartStrategy
  -- | Terminate all workers threads when one fails and restart them all
  = AllForOne

  -- | Only restart worker thread that failed
  | OneForOne
  deriving (Generic, Show, Eq, Ord)

instance Default SupervisorRestartStrategy where
  def = OneForOne

instance NFData SupervisorRestartStrategy

-- | Utility record used to specify options to a "Capataz" instance
data CapatazOptions
  = CapatazOptions {
    -- | Name of the Capataz (present on "CapatazEvent" records)
    supervisorName                    :: Text
    -- | How many errors is the Capataz be able to handle; check:
    -- http://erlang.org/doc/design_principles/sup_princ.html#max_intensity
  , supervisorIntensity               :: !Int
    -- | Period of time where the Capataz can receive "capatazIntensity" amount
    -- of errors
  , supervisorPeriodSeconds           :: !NominalDiffTime
    -- | What is the "SupervisorRestartStrategy" for this Capataz
  , supervisorRestartStrategy         :: !SupervisorRestartStrategy
    -- | Static set of processes that start as soon as the "Capataz" is created
  , supervisorProcessSpecList         :: ![ProcessSpec]
    -- | In which order the "Capataz" record is going to terminate it's processes
  , supervisorProcessTerminationOrder :: !ProcessTerminationOrder
    -- | Callback used when the error intensity is reached
  , supervisorOnIntensityReached      :: !(IO ())
    -- | ...
  , supervisorOnFailure               :: !(SomeException -> IO ())
    -- | Callback used for telemetry purposes
  , notifyEvent                       :: !(CapatazEvent -> IO ())
  }


-- | Utility record used to specify options to a "Worker" instance
data WorkerOptions
  = WorkerOptions {
    -- | Name of the Worker (present on "CapatazEvent" records)
    workerName              :: !WorkerName
    -- | Callback used when the worker fails with an error
  , workerOnFailure         :: !(SomeException -> IO ())
    -- | Callback used when the worker completes execution without error
  , workerOnCompletion      :: !(IO ())
    -- | Callback used when the worker is terminated
  , workerOnTermination     :: !(IO ())
    -- | Indicates how a worker should be terminated
  , workerTerminationPolicy :: !WorkerTerminationPolicy
    -- | Indicates how a worker should be restarted
  , workerRestartStrategy   :: !WorkerRestartStrategy
  }
  deriving (Generic)

-- | Specifies how a "Worker" should restart on failure. Default is "Transient"
data WorkerRestartStrategy
  -- | Worker thread is __always__ restarted
  = Permanent

  -- | Worker thread is restarted only if it failed
  | Transient

  -- | Worker thread is __never__ restarted
  | Temporary

  deriving (Generic, Show, Eq)

instance NFData WorkerRestartStrategy
instance Default WorkerRestartStrategy where
  def = Transient

-- | WorkerSpec is a representation of the "WorkerOptions" record that embeds
-- the @"IO" ()@ sub-routine of the worker thread. This record is used when we
-- want to bound worker threads to a "Capataz" instance
data WorkerSpec
  = WorkerSpec {
    -- | An @IO ()@ sub-routine that will be executed when the worker
    -- thread is created, this attribute is lazy given we want to this
    -- value on a worker thread environment.
    workerAction            :: WorkerAction
    -- | Name of the Worker (present on "CapatazEvent" records)
  , workerName              :: !WorkerName
    -- | Callback used when the worker fails with an error
  , workerOnFailure         :: !(SomeException -> IO ())
    -- | Callback used when the worker completes execution without error
  , workerOnCompletion      :: !(IO ())
    -- | Callback used when the worker is terminated
  , workerOnTermination     :: !(IO ())
    -- | Indicates how a worker should be terminated
  , workerTerminationPolicy :: !WorkerTerminationPolicy
    -- | Indicates how a worker should be restarted
  , workerRestartStrategy   :: !WorkerRestartStrategy
  }
  deriving (Generic)

-- | Record that contains the "Async" record (thread reference) of a worker
data Worker
  = Worker {
    -- | Unique identifier for a worker that is executing
    workerId           :: !WorkerId
    -- | "Async" thread of a worker, this Async executes the @IO ()@ sub-routine
  , workerAsync        :: !(Async ())
    -- | Time where this worker was created (used for error intensity checks)
  , workerCreationTime :: !UTCTime
    -- | Name of the Worker (present on "CapatazEvent" records)
  , workerName         :: !WorkerName
    -- | "WorkerSpec" contains all the options around restart and termination
    -- policies
  , workerSpec         :: !WorkerSpec
  }

-- | Convenience utility record that contains all values related to a "Worker";
-- this is used on internal functions of the Capataz library.
data WorkerEnv
  = WorkerEnv {
    workerAction          :: WorkerAction
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

data SupervisorSpec
  = SupervisorSpec {
    -- | Name of the Capataz (present on "CapatazEvent" records)
    supervisorName                    :: Text
    -- | How many errors is the Capataz be able to handle; check:
    -- http://erlang.org/doc/design_principles/sup_princ.html#max_intensity
  , supervisorIntensity               :: !Int
    -- | Period of time where the Capataz can receive "capatazIntensity" amount
    -- of errors
  , supervisorPeriodSeconds           :: !NominalDiffTime
    -- | What is the "SupervisorRestartStrategy" for this Capataz
  , supervisorRestartStrategy         :: !SupervisorRestartStrategy
    -- | Static set of workers that start as soon as the "Capataz" is created
  , supervisorProcessSpecList         :: ![ProcessSpec]
    -- | In which order the "Capataz" record is going to terminate it's workers
  , supervisorProcessTerminationOrder :: !ProcessTerminationOrder
    -- | Callback used when the error intensity is reached
  , supervisorOnIntensityReached      :: !(IO ())
  , supervisorOnFailure               :: !(SomeException -> IO ())
  }

data Supervisor
  = Supervisor {
    supervisorId           :: !SupervisorId
  , supervisorName         :: !SupervisorName
  , supervisorCreationTime :: !UTCTime
  , supervisorSpec         :: !SupervisorSpec
  , supervisorAsync        :: !(Async ())
  , supervisorNotify       :: (SupervisorMessage -> IO ())
  , supervisorEnv          :: !SupervisorEnv
  }

-- | Internal record that represents an action being sent from threads using
-- the Capataz public API.
data ControlAction
  = ForkWorker {
    workerSpec     :: !WorkerSpec
  , returnWorkerId :: !(WorkerId -> IO ())
  }
  | TerminateProcess {
    processId                :: !ProcessId
  , processTerminationReason :: !Text
  , notifyProcessTermination :: !(IO ())
  }
  | TerminateCapataz {
    notifyCapatazTermination :: !(IO ())
  }
  deriving (Generic)

-- | Internal exception thrown to the Capataz loop to indicate termination of
-- execution
data CapatazSignal
  = CapatazFailure
  | RestartProcessException
  | TerminateProcessException {
      processId                :: !ProcessId
    , processTerminationReason :: !Text
    }
  | BrutallyTerminateProcessException {
      processId                :: !ProcessId
    , processTerminationReason :: !Text
    }
    deriving (Generic, Show)

instance Exception CapatazSignal
instance NFData CapatazSignal

-- | Internal exception triggered when a Worker violates error intensity
-- specification
data CapatazError
  = CapatazIntensityReached {
    processId           :: !ProcessId
  , processName         :: !ProcessName
  , processRestartCount :: !Int
  }
  deriving (Generic, Show)

instance Exception CapatazError
instance NFData CapatazError

-- | Internal record that indicates what type of callback function is being
-- invoked; this is used for telemetry purposes
data CallbackType
  = OnCompletion
  | OnFailure
  | OnTermination
  deriving (Generic, Show, Eq)

-- | Internal exception triggered when a callback of a Worker fails
data ProcessError
  = ProcessCallbackFailed {
      processId            :: !WorkerId
    , processError         :: !(Maybe SomeException)
    , processCallbackError :: !SomeException
    , processCallbackType  :: !CallbackType
    }
    deriving (Generic, Show)

instance Exception ProcessError

-- | Internal event delivered from Worker threads to the Capataz thread to
-- indicate completion, failure or termination
data MonitorEvent
  = ProcessTerminated' {
    processId                :: !ProcessId
  , processName              :: !ProcessName
  , processRestartCount      :: !RestartCount
  , processTerminationReason :: !Text
  , monitorEventTime         :: !UTCTime
  }
  | ProcessFailed' {
    processId           :: !WorkerId
  , processName         :: !WorkerName
  , processRestartCount :: !RestartCount
  , processError        :: !SomeException
  , monitorEventTime    :: !UTCTime
  }
  | ProcessCompleted' {
    processId        :: !ProcessId
  , processName      :: !ProcessName
  , monitorEventTime :: !UTCTime
  }
  | ProcessForcedRestart {
    processId        :: !ProcessId
  , processName      :: !ProcessName
  , monitorEventTime :: !UTCTime
  }
  deriving (Show)

-- | Internal state machine record that indicates the state of a Capataz
data CapatazStatus
  -- | This state is set when Worker is created and it spawn static worker
  -- threads
  = Initializing
  -- | This state is set when the Capataz thread is listenting to both
  -- "ControlAction" and "MonitorEvent" messages
  | Running
  -- | This state is set when the Capataz thread is terminating it's assigned
  -- worker
  | Halting
  -- | The Capataz thread is done
  | Halted
  deriving (Generic, Show, Eq)

instance NFData CapatazStatus

-- | Internal message delivered to a Capataz thread that can either be a call
-- from public API or an event from a monitored Worker
data SupervisorMessage
  = ControlAction !ControlAction
  | MonitorEvent !MonitorEvent
  deriving (Generic)

data Process
  = WorkerProcess  Worker
  | SupervisorProcess Supervisor

data ProcessSpec
  = WorkerProcessSpec WorkerSpec
  | SupervisorProcessSpec SupervisorSpec

-- | Record that contains the environment of a capataz monitor, this is used as
-- the main record to create workers and to stop the supervisor thread.
data Capataz
  = Capataz {
    capatazSupervisor :: !Supervisor
  , capatazTeardown   :: !Teardown
  }

instance ITeardown Capataz where
  teardown Capataz {capatazTeardown} =
    teardown capatazTeardown

-- | Internal record used to hold part of the runtime information of a "Capataz"
-- record
data CapatazRuntime
  = CapatazRuntime {
    capatazId           :: !CapatazId
  , capatazCreationTime :: !UTCTime
  , capatazQueue        :: !(TQueue SupervisorMessage)
  , capatazProcessMap   :: !(IORef ProcessMap)
  , capatazStatusVar    :: !(TVar CapatazStatus)
  , capatazOptions      :: !CapatazOptions
  }

-- | Convenience utility record that contains all values related to a "Capataz";
-- this is used on internal functions of the Capataz library.
data CapatazEnv
  = CapatazEnv {
    capatazId                      :: !CapatazId
  , capatazName                    :: !CapatazName
  , capatazQueue                   :: !(TQueue SupervisorMessage)
  , capatazProcessMap              :: !(IORef ProcessMap)
  , capatazStatusVar               :: !(TVar CapatazStatus)
  , capatazOptions                 :: !CapatazOptions
  , capatazRuntime                 :: !CapatazRuntime
  , capatazIntensity               :: !Int
    -- ^ http://erlang.org/doc/design_principles/sup_princ.html#max_intensity
  , capatazPeriodSeconds           :: !NominalDiffTime
  , capatazRestartStrategy         :: !SupervisorRestartStrategy
  , capatazProcessTerminationOrder :: !ProcessTerminationOrder
  , capatazOnIntensityReached      :: !(IO ())
  , notifyEvent                    :: !(CapatazEvent -> IO ())
  }


data ParentSupervisorEnv
  = ParentSupervisorEnv {
    supervisorId     :: !SupervisorId
  , supervisorName   :: !SupervisorName
  , supervisorNotify :: !(SupervisorMessage -> IO ())
  , notifyEvent      :: !(CapatazEvent -> IO ())
  }

-- | Convenience utility record that contains all values related to a "Capataz";
-- this is used on internal functions of the Capataz library.
data SupervisorEnv
  = SupervisorEnv {
    supervisorId                      :: !SupervisorId
  , supervisorName                    :: !SupervisorName
  , supervisorNotify                  :: !(SupervisorMessage -> IO ())
  , supervisorGetNotification         :: !(STM SupervisorMessage)
  , supervisorProcessMap              :: !(IORef ProcessMap)
  , supervisorStatusVar               :: !(TVar CapatazStatus)
  , supervisorSpec                    :: !SupervisorSpec
  , supervisorIntensity               :: !Int
    -- ^ http://erlang.org/doc/design_principles/sup_princ.html#max_intensity
  , supervisorPeriodSeconds           :: !NominalDiffTime
  , supervisorRestartStrategy         :: !SupervisorRestartStrategy
  , supervisorProcessTerminationOrder :: !ProcessTerminationOrder
  , supervisorOnIntensityReached      :: !(IO ())
  , supervisorOnIntensityReached      :: !(SomeException -> IO ())
  , notifyEvent                       :: !(CapatazEvent -> IO ())
  }


-- | Default options to easily create capataz instances:
-- * name defaults to \"default-capataz\"
-- * intensity error tolerance is set to 1 error every 5 seconds
-- * has a "OneForOne " capataz restart strategy
-- * has a termination order of "OldestFirst"
defCapatazOptions :: CapatazOptions
defCapatazOptions = CapatazOptions
  { supervisorName                    = "capataz-root"

  -- One (1) restart every five (5) seconds
  , supervisorIntensity               = 1
  , supervisorPeriodSeconds           = 5
  , supervisorRestartStrategy         = def
  , supervisorProcessSpecList         = []
  , supervisorProcessTerminationOrder = OldestFirst
  , supervisorOnIntensityReached      = return ()
  , supervisorOnFailure               = const $ return ()
  , notifyEvent                       = const $ return ()
  }

-- | Default options to easily create supervisor instances:
-- * name defaults to \"default-capataz\"
-- * intensity error tolerance is set to 1 error every 5 seconds
-- * has a "OneForOne " capataz restart strategy
-- * has a termination order of "OldestFirst"
defSupervisorSpec :: SupervisorSpec
defSupervisorSpec = SupervisorSpec
  { supervisorName                    = "default-supervisor"

  -- One (1) restart every five (5) seconds
  , supervisorIntensity               = 1
  , supervisorPeriodSeconds           = 5
  , supervisorRestartStrategy         = def
  , supervisorProcessSpecList         = []
  , supervisorProcessTerminationOrder = OldestFirst
  , supervisorOnIntensityReached      = return ()
  , supervisorOnFailure               = const $ return ()
  }

-- | Default options to easily create worker instances:
-- * name defaults to \"default-worker\"
-- * has a "Transient" worker restart strategy
-- * has a termination policy of three (3) seconds
defWorkerOptions :: WorkerOptions
defWorkerOptions = WorkerOptions
  { workerName              = "default-worker"
  , workerOnFailure         = const $ return ()
  , workerOnCompletion      = return ()
  , workerOnTermination     = return ()
  , workerTerminationPolicy = def
  , workerRestartStrategy   = def
  }

-- | Default spec to easily create worker instances:
-- * @IO ()@ sub-routine simply returns unit
-- * name defaults to \"default-worker\"
-- * has a "Transient" worker restart strategy
-- * has a termination policy of three (3) seconds
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

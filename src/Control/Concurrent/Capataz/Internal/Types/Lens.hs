{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Control.Concurrent.Capataz.Internal.Types.Lens where

import RIO

import Control.Concurrent.Capataz.Internal.Types
import Data.Time.Clock                           (NominalDiffTime)

--------------------------------------------------------------------------------

class HasSupervisorIntensity s where
  -- | Specifies how many errors is a supervisor able to handle; check:
  -- http://erlang.org/doc/design_principles/sup_princ.html#max_intensity.
  supervisorIntensityL
    :: Functor f
    => (Int -> f Int)
    -> s
    -> f s

class HasSupervisorPeriodSeconds s where
  -- | Specifies period of time in which a supervisor can receive a number of
  -- errors specified in "supervisorIntensityL".
  supervisorPeriodSecondsL
    :: Functor f
    => (NominalDiffTime -> f NominalDiffTime)
    -> s
    -> f s

class HasSupervisorRestartStrategy s where
  -- | Specifies the "SupervisorRestartStrategy" for a root supervisor.
  supervisorRestartStrategyL
    :: Functor f
    => (SupervisorRestartStrategy -> f SupervisorRestartStrategy)
    -> s
    -> f s

class HasSupervisorProcessSpecList s where
  -- | Specifies a static list of processes that start automatically with a
  -- supervisor.
  supervisorProcessSpecListL
    :: Functor f
    => ([ProcessSpec] -> f [ProcessSpec])
    -> s
    -> f s

class HasSupervisorProcessTerminationOrder s where
  -- | Specifies order in which a supervisor is going to terminate its
  -- supervised processes.
  supervisorProcessTerminationOrderL
    :: Functor f
    => (ProcessTerminationOrder -> f ProcessTerminationOrder)
    -> s
    -> f s

class HasSupervisorIntensityReachedCallback s where
  -- | Specifies a callback sub-routine that gets executed when there is a
  -- breach in a supervisor's error intensity.
  supervisorOnIntensityReachedL
    :: Functor f
    => (IO () -> f (IO ()))
    -> s
    -> f s

class HasSupervisorFailureCallback s where
  -- | Specifies callback sub-routine that gets executed when a supervisor
  -- fails.
  supervisorOnFailureL
    :: Functor f
    => ((SomeException -> IO ()) -> f (SomeException -> IO ()))
    -> s
    -> f s


instance HasSupervisorIntensity SupervisorOptions where
  supervisorIntensityL k SupervisorOptions {supervisorIntensity, ..} =
    fmap (\newSupIntensity -> SupervisorOptions { supervisorIntensity = newSupIntensity, .. })
         (k supervisorIntensity)

instance HasSupervisorPeriodSeconds SupervisorOptions where
  supervisorPeriodSecondsL k SupervisorOptions {supervisorPeriodSeconds, ..} =
    fmap (\newSupPeriodSeconds -> SupervisorOptions { supervisorPeriodSeconds = newSupPeriodSeconds, .. })
         (k supervisorPeriodSeconds)

instance HasSupervisorRestartStrategy SupervisorOptions where
  supervisorRestartStrategyL k SupervisorOptions {supervisorRestartStrategy, ..} =
    fmap (\newSupRestartStrategy ->
            SupervisorOptions { supervisorRestartStrategy = newSupRestartStrategy, .. })
         (k supervisorRestartStrategy)

instance HasSupervisorProcessSpecList SupervisorOptions where
  supervisorProcessSpecListL k SupervisorOptions {supervisorProcessSpecList, ..} =
    fmap (\newSupProcessSpecList ->
            SupervisorOptions { supervisorProcessSpecList = newSupProcessSpecList, .. })
         (k supervisorProcessSpecList)

instance HasSupervisorProcessTerminationOrder SupervisorOptions where
  supervisorProcessTerminationOrderL k SupervisorOptions {supervisorProcessTerminationOrder, ..} =
    fmap (\newSupProcessTerminationOrder ->
            SupervisorOptions { supervisorProcessTerminationOrder = newSupProcessTerminationOrder, .. })
         (k supervisorProcessTerminationOrder)

instance HasSupervisorIntensityReachedCallback SupervisorOptions where
  supervisorOnIntensityReachedL k SupervisorOptions {supervisorOnIntensityReached, ..} =
    fmap (\newSupOnIntensityReached -> SupervisorOptions { supervisorOnIntensityReached = newSupOnIntensityReached, .. })
         (k supervisorOnIntensityReached)

instance HasSupervisorFailureCallback SupervisorOptions where
  supervisorOnFailureL k SupervisorOptions {supervisorOnFailure, ..} =
    fmap (\newSupOnFailure -> SupervisorOptions { supervisorOnFailure = newSupOnFailure, .. })
         (k supervisorOnFailure)

--------------------

instance HasSupervisorIntensity CapatazOptions where
  supervisorIntensityL k CapatazOptions {supervisorIntensity, ..} =
    fmap (\newSupIntensity -> CapatazOptions { supervisorIntensity = newSupIntensity, .. })
         (k supervisorIntensity)

instance HasSupervisorPeriodSeconds CapatazOptions where
  supervisorPeriodSecondsL k CapatazOptions {supervisorPeriodSeconds, ..} =
    fmap (\newSupPeriodSeconds -> CapatazOptions { supervisorPeriodSeconds = newSupPeriodSeconds, .. })
         (k supervisorPeriodSeconds)

instance HasSupervisorRestartStrategy CapatazOptions where
  supervisorRestartStrategyL k CapatazOptions {supervisorRestartStrategy, ..} =
    fmap (\newSupRestartStrategy ->
            CapatazOptions { supervisorRestartStrategy = newSupRestartStrategy, .. })
         (k supervisorRestartStrategy)

instance HasSupervisorProcessSpecList CapatazOptions where
  supervisorProcessSpecListL k CapatazOptions {supervisorProcessSpecList, ..} =
    fmap (\newSupProcessSpecList ->
            CapatazOptions { supervisorProcessSpecList = newSupProcessSpecList, .. })
         (k supervisorProcessSpecList)

instance HasSupervisorProcessTerminationOrder CapatazOptions where
  supervisorProcessTerminationOrderL k CapatazOptions {supervisorProcessTerminationOrder, ..} =
    fmap (\newSupProcessTerminationOrder ->
            CapatazOptions { supervisorProcessTerminationOrder = newSupProcessTerminationOrder, .. })
         (k supervisorProcessTerminationOrder)

instance HasSupervisorIntensityReachedCallback CapatazOptions where
  supervisorOnIntensityReachedL k CapatazOptions {supervisorOnIntensityReached, ..} =
    fmap (\newSupOnIntensityReached -> CapatazOptions { supervisorOnIntensityReached = newSupOnIntensityReached, .. })
         (k supervisorOnIntensityReached)

instance HasSupervisorFailureCallback CapatazOptions where
  supervisorOnFailureL k CapatazOptions {supervisorOnFailure, ..} =
    fmap (\newSupOnFailure -> CapatazOptions { supervisorOnFailure = newSupOnFailure, .. })
         (k supervisorOnFailure)

  -- | Specifies a callback sub-routine that gets triggered everytime something
  -- important happens on the capataz system. This callback should be used for
  -- telemetry purposes (e.g. logging, monitoring, etc).
onSystemEventL
  :: Functor f
  => ((CapatazEvent -> IO ()) -> f (CapatazEvent -> IO ()))
  -> CapatazOptions
  -> f CapatazOptions
onSystemEventL k CapatazOptions { notifyEvent, ..} = fmap
  (\newNotifyEvent -> CapatazOptions {notifyEvent = newNotifyEvent, ..})
  (k notifyEvent)

--------------------

-- | Specifies callback that gets executed when worker sub-routine has runtime
-- error.
--
-- NOTE: the given sub-routine execution may be interrupted depending on the
-- worker "WorkerTerminationPolicy".
--
workerOnFailureL
  :: Functor f
  => ((SomeException -> IO ()) -> f (SomeException -> IO ()))
  -> WorkerOptions
  -> f WorkerOptions
workerOnFailureL k WorkerOptions { workerOnFailure, ..} = fmap
  (\newWorkerAction -> WorkerOptions {workerOnFailure = newWorkerAction, ..})
  (k workerOnFailure)

-- | Specifies callback that gets executed when worker sub-routine completes
-- with no errors.
--
-- NOTE: the given sub-routine execution may be interrupted depending on the
-- worker "WorkerTerminationPolicy".
--
workerOnCompletionL
  :: Functor f => (IO () -> f (IO ())) -> WorkerOptions -> f WorkerOptions
workerOnCompletionL k WorkerOptions { workerOnCompletion, ..} = fmap
  (\newWorkerAction -> WorkerOptions {workerOnCompletion = newWorkerAction, ..})
  (k workerOnCompletion)

-- | Specifies callback that gets executed when worker sub-routine is terminated
-- by its supervisor; this may happen in case of a capataz system shutdown or
-- when there is an "AllForOne" restart policy in place.
--
-- NOTE: the given sub-routine execution may be interrupted depending on the
-- worker "WorkerTerminationPolicy".
--
workerOnTerminationL
  :: Functor f => (IO () -> f (IO ())) -> WorkerOptions -> f WorkerOptions
workerOnTerminationL k WorkerOptions { workerOnTermination, ..} = fmap
  ( \newWorkerAction ->
    WorkerOptions {workerOnTermination = newWorkerAction, ..}
  )
  (k workerOnTermination)

-- | Specifies how to handle a worker termination. See "WorkerTerminationPolicy"
-- documentation for more details.
workerTerminationPolicyL
  :: Functor f
  => (WorkerTerminationPolicy -> f WorkerTerminationPolicy)
  -> WorkerOptions
  -> f WorkerOptions
workerTerminationPolicyL k WorkerOptions { workerTerminationPolicy, ..} = fmap
  ( \newWorkerAction ->
    WorkerOptions {workerTerminationPolicy = newWorkerAction, ..}
  )
  (k workerTerminationPolicy)

-- | Specifies how supervisor should deal with an error when worker fails or
-- completes. See "WorkerRestartStrategy" documentation for more details.
workerRestartStrategyL
  :: Functor f
  => (WorkerRestartStrategy -> f WorkerRestartStrategy)
  -> WorkerOptions
  -> f WorkerOptions
workerRestartStrategyL k WorkerOptions { workerRestartStrategy, ..} = fmap
  ( \newWorkerAction ->
    WorkerOptions {workerRestartStrategy = newWorkerAction, ..}
  )
  (k workerRestartStrategy)

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Control.Concurrent.Capataz.Internal.Types.Lens where

import Control.Concurrent.Capataz.Internal.Types
import Data.Time.Clock                           (NominalDiffTime)
import Protolude

--------------------------------------------------------------------------------

class SupervisorOptionLens s where

  supervisorNameL
    :: Functor f
    => (Text -> f Text)
    -> s
    -> f s
  supervisorIntensityL
    :: Functor f
    => (Int -> f Int)
    -> s
    -> f s
  supervisorPeriodSecondsL
    :: Functor f
    => (NominalDiffTime -> f NominalDiffTime)
    -> s
    -> f s

  supervisorRestartStrategyL
    :: Functor f
    => (SupervisorRestartStrategy -> f SupervisorRestartStrategy)
    -> s
    -> f s

  supervisorProcessSpecListL
    :: Functor f
    => ([ProcessSpec] -> f [ProcessSpec])
    -> s
    -> f s

  supervisorProcessTerminationOrderL
    :: Functor f
    => (ProcessTerminationOrder -> f ProcessTerminationOrder)
    -> s
    -> f s

  supervisorOnIntensityReachedL
    :: Functor f
    => (IO () -> f (IO ()))
    -> s
    -> f s

  supervisorOnFailureL
    :: Functor f
    => ((SomeException -> IO ()) -> f (SomeException -> IO ()))
    -> s
    -> f s

instance SupervisorOptionLens SupervisorOptions where
  supervisorNameL k SupervisorOptions {supervisorName, ..} =
    fmap (\newSupName -> SupervisorOptions { supervisorName = newSupName, .. })
         (k supervisorName)

  supervisorIntensityL k SupervisorOptions {supervisorIntensity, ..} =
    fmap (\newSupIntensity -> SupervisorOptions { supervisorIntensity = newSupIntensity, .. })
         (k supervisorIntensity)

  supervisorPeriodSecondsL k SupervisorOptions {supervisorPeriodSeconds, ..} =
    fmap (\newSupPeriodSeconds -> SupervisorOptions { supervisorPeriodSeconds = newSupPeriodSeconds, .. })
         (k supervisorPeriodSeconds)

  supervisorRestartStrategyL k SupervisorOptions {supervisorRestartStrategy, ..} =
    fmap (\newSupRestartStrategy ->
            SupervisorOptions { supervisorRestartStrategy = newSupRestartStrategy, .. })
         (k supervisorRestartStrategy)

  supervisorProcessSpecListL k SupervisorOptions {supervisorProcessSpecList, ..} =
    fmap (\newSupProcessSpecList ->
            SupervisorOptions { supervisorProcessSpecList = newSupProcessSpecList, .. })
         (k supervisorProcessSpecList)

  supervisorProcessTerminationOrderL k SupervisorOptions {supervisorProcessTerminationOrder, ..} =
    fmap (\newSupProcessTerminationOrder ->
            SupervisorOptions { supervisorProcessTerminationOrder = newSupProcessTerminationOrder, .. })
         (k supervisorProcessTerminationOrder)

  supervisorOnIntensityReachedL k SupervisorOptions {supervisorOnIntensityReached, ..} =
    fmap (\newSupOnIntensityReached -> SupervisorOptions { supervisorOnIntensityReached = newSupOnIntensityReached, .. })
         (k supervisorOnIntensityReached)

  supervisorOnFailureL k SupervisorOptions {supervisorOnFailure, ..} =
    fmap (\newSupOnFailure -> SupervisorOptions { supervisorOnFailure = newSupOnFailure, .. })
         (k supervisorOnFailure)

instance SupervisorOptionLens CapatazOptions where
  supervisorNameL k CapatazOptions {supervisorName, ..} =
    fmap (\newSupName -> CapatazOptions { supervisorName = newSupName, .. })
         (k supervisorName)

  supervisorIntensityL k CapatazOptions {supervisorIntensity, ..} =
    fmap (\newSupIntensity -> CapatazOptions { supervisorIntensity = newSupIntensity, .. })
         (k supervisorIntensity)

  supervisorPeriodSecondsL k CapatazOptions {supervisorPeriodSeconds, ..} =
    fmap (\newSupPeriodSeconds -> CapatazOptions { supervisorPeriodSeconds = newSupPeriodSeconds, .. })
         (k supervisorPeriodSeconds)

  supervisorRestartStrategyL k CapatazOptions {supervisorRestartStrategy, ..} =
    fmap (\newSupRestartStrategy ->
            CapatazOptions { supervisorRestartStrategy = newSupRestartStrategy, .. })
         (k supervisorRestartStrategy)

  supervisorProcessSpecListL k CapatazOptions {supervisorProcessSpecList, ..} =
    fmap (\newSupProcessSpecList ->
            CapatazOptions { supervisorProcessSpecList = newSupProcessSpecList, .. })
         (k supervisorProcessSpecList)

  supervisorProcessTerminationOrderL k CapatazOptions {supervisorProcessTerminationOrder, ..} =
    fmap (\newSupProcessTerminationOrder ->
            CapatazOptions { supervisorProcessTerminationOrder = newSupProcessTerminationOrder, .. })
         (k supervisorProcessTerminationOrder)

  supervisorOnIntensityReachedL k CapatazOptions {supervisorOnIntensityReached, ..} =
    fmap (\newSupOnIntensityReached -> CapatazOptions { supervisorOnIntensityReached = newSupOnIntensityReached, .. })
         (k supervisorOnIntensityReached)

  supervisorOnFailureL k CapatazOptions {supervisorOnFailure, ..} =
    fmap (\newSupOnFailure -> CapatazOptions { supervisorOnFailure = newSupOnFailure, .. })
         (k supervisorOnFailure)


notifyEventL
  :: Functor f
  => ((CapatazEvent -> IO ()) -> f (CapatazEvent -> IO ()))
  -> CapatazOptions
  -> f CapatazOptions
notifyEventL k CapatazOptions { notifyEvent, ..} = fmap
  (\newNotifyEvent -> CapatazOptions {notifyEvent = newNotifyEvent, ..})
  (k notifyEvent)

workerActionL
  :: Functor f
  => (WorkerAction -> f WorkerAction)
  -> WorkerOptions
  -> f WorkerOptions
workerActionL k WorkerOptions { workerAction, ..} = fmap
  (\newWorkerAction -> WorkerOptions {workerAction = newWorkerAction, ..})
  (k workerAction)

workerNameL
  :: Functor f
  => (WorkerName -> f WorkerName)
  -> WorkerOptions
  -> f WorkerOptions
workerNameL k WorkerOptions { workerName, ..} = fmap
  (\newWorkerAction -> WorkerOptions {workerName = newWorkerAction, ..})
  (k workerName)

workerOnFailureL
  :: Functor f
  => ((SomeException -> IO ()) -> f (SomeException -> IO ()))
  -> WorkerOptions
  -> f WorkerOptions
workerOnFailureL k WorkerOptions { workerOnFailure, ..} = fmap
  (\newWorkerAction -> WorkerOptions {workerOnFailure = newWorkerAction, ..})
  (k workerOnFailure)

workerOnCompletionL
  :: Functor f => (IO () -> f (IO ())) -> WorkerOptions -> f WorkerOptions
workerOnCompletionL k WorkerOptions { workerOnCompletion, ..} = fmap
  (\newWorkerAction -> WorkerOptions {workerOnCompletion = newWorkerAction, ..})
  (k workerOnCompletion)

workerOnTerminationL
  :: Functor f => (IO () -> f (IO ())) -> WorkerOptions -> f WorkerOptions
workerOnTerminationL k WorkerOptions { workerOnTermination, ..} = fmap
  ( \newWorkerAction ->
    WorkerOptions {workerOnTermination = newWorkerAction, ..}
  )
  (k workerOnTermination)

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

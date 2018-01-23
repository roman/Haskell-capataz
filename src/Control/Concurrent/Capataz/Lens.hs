module Control.Concurrent.Capataz.Lens
  ( module X )
  where

import Lens.Micro as X ((^.), (&), (.~), set)
import Lens.Micro.Extras as X (view)
import Control.Concurrent.Capataz.Internal.Types.Lens as X
  (
    supervisorNameL
  , supervisorIntensityL
  , supervisorPeriodSecondsL
  , supervisorRestartStrategyL
  , supervisorProcessSpecListL
  , supervisorProcessTerminationOrderL
  , supervisorOnIntensityReachedL
  , supervisorOnFailureL
  , notifyEventL
  , workerActionL
  , workerNameL
  , workerOnFailureL
  , workerOnCompletionL
  , workerOnTerminationL
  , workerTerminationPolicyL
  , workerRestartStrategyL
  )

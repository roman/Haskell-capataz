{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}

-- | This module contains functionality that works both for Supervisor and
-- Worker process types.
module Control.Concurrent.Capataz.Internal.Process where

import RIO

import Control.Concurrent.Capataz.Internal.Types
import Control.Concurrent.Capataz.Internal.Util
    (readProcessMap, sortProcessesByTerminationOrder)
import RIO.Time                                  (UTCTime, getCurrentTime)

-- | Gets "Async" from a given Process.
getProcessAsync :: Process m -> Async ()
getProcessAsync process = case process of
  WorkerProcess     Worker { workerAsync }         -> workerAsync
  SupervisorProcess Supervisor { supervisorAsync } -> supervisorAsync

-- | Gets "ThreadId" from a given Process.
getProcessThreadId :: Process m -> ProcessThreadId
getProcessThreadId = PTID . asyncThreadId . getProcessAsync

-- | Gets "ProcessId" from a given Process.
getProcessId :: Process m -> ProcessId
getProcessId process = case process of
  WorkerProcess     Worker { workerId }         -> workerId
  SupervisorProcess Supervisor { supervisorId } -> supervisorId

-- | Gets "ProcessName" from a given "ProcessSpec".
getProcessName :: ProcessSpec m -> ProcessName
getProcessName procSpec = case procSpec of
  WorkerSpec     WorkerOptions { workerName }         -> workerName
  SupervisorSpec SupervisorOptions { supervisorName } -> supervisorName

-- | Gets "ProcessType" from a given "ProcessSpec".
getProcessType :: ProcessSpec m -> ProcessType
getProcessType processSpec = case processSpec of
  WorkerSpec{}     -> WorkerType
  SupervisorSpec{} -> SupervisorType

-- | Gets "ProcessSpec" of a given "Process".
getProcessSpec :: Process m -> ProcessSpec m
getProcessSpec process = case process of
  WorkerProcess Worker { workerOptions } -> WorkerSpec workerOptions
  SupervisorProcess Supervisor { supervisorOptions } ->
    SupervisorSpec supervisorOptions

-- | Utility function to send notifications to the capataz system callback when
-- a Process fails.
notifyProcessFailed
  :: MonadIO m => SupervisorEnv m -> Process m -> SomeException -> m ()
notifyProcessFailed SupervisorEnv { supervisorId, supervisorName, notifyEvent } process processError
  = do
    eventTime <- getCurrentTime
    notifyEvent ProcessFailed
      { supervisorId
      , supervisorName
      , processId       = getProcessId process
      , processName     = getProcessName (getProcessSpec process)
      , processType     = getProcessType (getProcessSpec process)
      , processThreadId = getProcessThreadId process
      , processError
      , eventTime
      }

-- | Utility function to send notifications to the capataz system callback when
-- a Process is terminated.
notifyProcessTerminated
  :: MonadIO m => SupervisorEnv m -> Process m -> Text -> m ()
notifyProcessTerminated SupervisorEnv { supervisorId, supervisorName, notifyEvent } process terminationReason
  = do
    eventTime <- getCurrentTime
    notifyEvent ProcessTerminated
      { supervisorId
      , supervisorName
      , processId         = getProcessId process
      , processName       = getProcessName (getProcessSpec process)
      , processType       = getProcessType (getProcessSpec process)
      , processThreadId   = getProcessThreadId process
      , terminationReason
      , eventTime
      }

-- | Utility function to send notifications to the capataz system callback when
-- a Process is started or restarted.
notifyProcessStarted
  :: MonadIO m
  => Maybe (ProcessId, RestartCount)
  -> ParentSupervisorEnv m
  -> Process m
  -> m ()
notifyProcessStarted mRestartInfo ParentSupervisorEnv { supervisorId, supervisorName, notifyEvent } process
  = do
    eventTime <- getCurrentTime
    case mRestartInfo of
      Just (_processId, processRestartCount) -> notifyEvent ProcessRestarted
        { supervisorId
        , supervisorName
        , processId           = getProcessId process
        , processName         = getProcessName (getProcessSpec process)
        , processType         = getProcessType (getProcessSpec process)
        , processThreadId     = getProcessThreadId process
        , processRestartCount
        , eventTime
        }
      Nothing -> notifyEvent ProcessStarted
        { supervisorId
        , supervisorName
        , processId       = getProcessId process
        , processName     = getProcessName (getProcessSpec process)
        , processType     = getProcessType (getProcessSpec process)
        , processThreadId = getProcessThreadId process
        , eventTime
        }

-- | Utility function to send notifications when a Process sub-routine completes
-- without errors.
notifyProcessCompleted
  :: SupervisorEnv m -> Process m -> UTCTime -> m ()
notifyProcessCompleted SupervisorEnv { supervisorId, supervisorName, notifyEvent } process eventTime
  = notifyEvent ProcessCompleted
    { supervisorId
    , supervisorName
    , processId       = getProcessId process
    , processName     = getProcessName (getProcessSpec process)
    , processType     = getProcessType (getProcessSpec process)
    , processThreadId = getProcessThreadId process
    , eventTime
    }


-- | Utility function to execute a Process onCompletion sub-routine.
callProcessOnCompletion :: Monad m => ProcessSpec m -> m ()
callProcessOnCompletion procSpec = case procSpec of
  WorkerSpec WorkerOptions { workerOnCompletion } -> workerOnCompletion
  _                                               -> return ()

-- | Utility function to execute a Process onFailure sub-routine.
callProcessOnFailure :: ProcessSpec m -> SomeException -> m ()
callProcessOnFailure procSpec err = case procSpec of
  WorkerSpec WorkerOptions { workerOnFailure } -> workerOnFailure err
  SupervisorSpec SupervisorOptions { supervisorOnFailure } ->
    supervisorOnFailure err

-- | Utility function to execute a Process onTermination sub-routine.
callProcessOnTermination :: Monad m => ProcessSpec m -> m ()
callProcessOnTermination procSpec = case procSpec of
  WorkerSpec WorkerOptions { workerOnTermination } -> workerOnTermination
  _                                                -> return ()

-- | Handles errors produced - or thrown to - a process thread.
handleProcessException
  :: (MonadUnliftIO m)
  => (m () -> m a)
  -> ParentSupervisorEnv m
  -> ProcessSpec m
  -> ProcessId
  -> RestartCount
  -> SomeException
  -> m MonitorEvent
handleProcessException unmask ParentSupervisorEnv { supervisorId, supervisorName, notifyEvent } procSpec processId restartCount err
  = do
    let processName = getProcessName procSpec
    processThreadId  <- PTID <$> myThreadId
    monitorEventTime <- getCurrentTime

    case fromAnyException err of
      Just RestartProcessException -> return ProcessForcedRestart
        { processId
        , processName
        , monitorEventTime
        }

      Just TerminateProcessException { processTerminationReason } -> do
        eErrResult <- unsafeTry $ unmask $ callProcessOnTermination procSpec

        notifyEvent ProcessCallbackExecuted
          { supervisorId
          , supervisorName
          , processThreadId
          , processId
          , processName
          , processType          = getProcessType procSpec
          , processCallbackError = either Just (const Nothing) eErrResult
          , processCallbackType  = OnTermination
          , eventTime            = monitorEventTime
          }


        case eErrResult of
          Left processCallbackError -> return ProcessFailed'
            { processId
            , processName
            , processError        = toException ProcessCallbackFailed
              { processId
              , processCallbackError
              , processCallbackType  = OnTermination
              , processError         = Just err
              }
            , processRestartCount = restartCount
            , monitorEventTime
            }
          Right _ -> return ProcessTerminated'
            { processId
            , processName
            , monitorEventTime
            , processTerminationReason
            , processRestartCount      = restartCount
            }

      Just BrutallyTerminateProcessException { processTerminationReason } ->
        return ProcessTerminated'
          { processId
          , processName
          , monitorEventTime
          , processTerminationReason
          , processRestartCount      = restartCount
          }

      _ -> do
        eErrResult <- unsafeTry $ unmask $ callProcessOnFailure procSpec err

        notifyEvent ProcessCallbackExecuted
          { supervisorId
          , supervisorName
          , processId
          , processName
          , processType          = getProcessType procSpec
          , processThreadId
          , processCallbackError = either Just (const Nothing) eErrResult
          , processCallbackType  = OnFailure
          , eventTime            = monitorEventTime
          }

        case eErrResult of
          Left processCallbackError -> return ProcessFailed'
            { processId
            , processName
            , monitorEventTime
            , processRestartCount = restartCount
            , processError        = toException ProcessCallbackFailed
              { processId
              , processCallbackError
              , processCallbackType  = OnFailure
              , processError         = Just err
              }
            }
          Right _ -> return ProcessFailed'
            { processId
            , processName
            , processError        = err
            , processRestartCount = restartCount
            , monitorEventTime
            }

-- | Handles completion of a Process sub-routine.
handleProcessCompletion
  :: (MonadUnliftIO m)
  => (m () -> m a)
  -> ParentSupervisorEnv m
  -> ProcessSpec m
  -> ProcessId
  -> RestartCount
  -> m MonitorEvent
handleProcessCompletion unmask ParentSupervisorEnv { supervisorId, supervisorName, notifyEvent } procSpec processId restartCount
  = do
    let processName = getProcessName procSpec
    processThreadId  <- PTID <$> myThreadId
    monitorEventTime <- getCurrentTime
    eCompResult      <- unsafeTry $ unmask $ callProcessOnCompletion procSpec

    notifyEvent ProcessCallbackExecuted
      { supervisorId
      , supervisorName
      , processId
      , processName
      , processType          = getProcessType procSpec
      , processThreadId
      , processCallbackError = either Just (const Nothing) eCompResult
      , processCallbackType  = OnCompletion
      , eventTime            = monitorEventTime
      }

    case eCompResult of
      Left err -> return ProcessFailed'
        { processId
        , processName
        , processError        = toException ProcessCallbackFailed
          { processId
          , processCallbackError = err
          , processError         = Nothing
          , processCallbackType  = OnCompletion
          }
        , processRestartCount = restartCount
        , monitorEventTime
        }
      Right _ ->
        return ProcessCompleted' {processName , processId , monitorEventTime }

-- | Internal utility function to trigger termination of a Process.
--
-- NOTE: The difference between public's API function and this, is that this
-- function gets executed on the supervisor's thread.
--
terminateProcess
  :: (MonadUnliftIO m)
  => Text -- ^ Description that indicates _why_ there is a termination
  -> SupervisorEnv m
  -> Process m
  -> m ()
terminateProcess processTerminationReason env process = do
  case process of
    WorkerProcess worker -> terminateWorker processTerminationReason worker
    SupervisorProcess supervisor ->
      terminateSupervisor processTerminationReason supervisor

  notifyProcessTerminated env process processTerminationReason

-- | Internal utility function that manages execution of a termination policy
-- for a Worker.
terminateWorker :: (MonadUnliftIO m) => Text -> Worker m -> m ()
terminateWorker processTerminationReason Worker { workerId, workerOptions, workerAsync }
  = do
    let processId = workerId
        WorkerOptions { workerTerminationPolicy } = workerOptions
    case workerTerminationPolicy of
      Infinity -> cancelWith
        workerAsync
        TerminateProcessException {processId , processTerminationReason }

      BrutalTermination -> cancelWith
        workerAsync
        BrutallyTerminateProcessException
          { processId
          , processTerminationReason
          }

      TimeoutMillis millis -> do
        -- NOTE: Given Teardown executes teardown operations in an uninterruptible mask
        -- we need to run asyncWithUnmask to come back to a unmasked state, sadly, race
        -- doesn't use asyncWithUnmask_, so we need to use it here
        result <- asyncWithUnmask $ \unmask -> unmask $ race_
          (do
            threadDelay (millis * 1000)
            cancelWith
              workerAsync
              BrutallyTerminateProcessException
                { processId
                , processTerminationReason
                }
          )
          (cancelWith
            workerAsync
            TerminateProcessException {processId , processTerminationReason }
          )
        wait result

-- | Internal utility function that manages execution of a termination policy
-- for a Supervisor.
terminateSupervisor :: MonadIO m => Text -> Supervisor m -> m ()
terminateSupervisor processTerminationReason Supervisor { supervisorId = processId, supervisorAsync }
  = cancelWith
    supervisorAsync
    TerminateProcessException {processId , processTerminationReason }

-- | Internal sub-routine that terminates workers of a supervisor, used when a
-- supervisor instance is terminated.
terminateProcessMap
  :: (MonadUnliftIO m) => Text -> SupervisorEnv m -> m ()
terminateProcessMap terminationReason env@SupervisorEnv { supervisorId, supervisorName, supervisorProcessTerminationOrder, notifyEvent }
  = do
    eventTime  <- getCurrentTime
    processMap <- readProcessMap env

    let processList = sortProcessesByTerminationOrder
          supervisorProcessTerminationOrder
          processMap

    notifyEvent ProcessTerminationStarted
      { supervisorId
      , supervisorName
      , terminationReason
      , eventTime
      }

    forM_ processList (terminateProcess terminationReason env)

    notifyEvent ProcessTerminationFinished
      { supervisorId
      , supervisorName
      , terminationReason
      , eventTime
      }

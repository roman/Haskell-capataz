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
import Data.Time.Clock                           (UTCTime, getCurrentTime)

-- | Gets "Async" from a given Process.
getProcessAsync :: Process -> Async ()
getProcessAsync process = case process of
  WorkerProcess     Worker { workerAsync }         -> workerAsync
  SupervisorProcess Supervisor { supervisorAsync } -> supervisorAsync

-- | Gets "ThreadId" from a given Process.
getProcessThreadId :: Process -> ThreadId
getProcessThreadId = asyncThreadId . getProcessAsync

-- | Gets "ProcessId" from a given Process.
getProcessId :: Process -> ProcessId
getProcessId process = case process of
  WorkerProcess     Worker { workerId }         -> workerId
  SupervisorProcess Supervisor { supervisorId } -> supervisorId

-- | Gets "ProcessName" from a given "ProcessSpec".
getProcessName :: ProcessSpec -> ProcessName
getProcessName procSpec = case procSpec of
  WorkerSpec     WorkerOptions { workerName }         -> workerName
  SupervisorSpec SupervisorOptions { supervisorName } -> supervisorName

-- | Gets "ProcessType" from a given "ProcessSpec".
getProcessType :: ProcessSpec -> ProcessType
getProcessType processSpec = case processSpec of
  WorkerSpec{}     -> WorkerType
  SupervisorSpec{} -> SupervisorType

-- | Gets "ProcessSpec" of a given "Process".
getProcessSpec :: Process -> ProcessSpec
getProcessSpec process = case process of
  WorkerProcess Worker { workerOptions } -> WorkerSpec workerOptions
  SupervisorProcess Supervisor { supervisorOptions } ->
    SupervisorSpec supervisorOptions

-- | Utility function to send notifications to the capataz system callback when
-- a Process fails.
notifyProcessFailed :: SupervisorEnv -> Process -> SomeException -> IO ()
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
notifyProcessTerminated :: SupervisorEnv -> Process -> Text -> IO ()
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
  :: Maybe (ProcessId, RestartCount) -> ParentSupervisorEnv -> Process -> IO ()
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
notifyProcessCompleted :: SupervisorEnv -> Process -> UTCTime -> IO ()
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
callProcessOnCompletion :: ProcessSpec -> IO ()
callProcessOnCompletion procSpec = case procSpec of
  WorkerSpec WorkerOptions { workerOnCompletion } -> workerOnCompletion
  _                                               -> return ()

-- | Utility function to execute a Process onFailure sub-routine.
callProcessOnFailure :: ProcessSpec -> SomeException -> IO ()
callProcessOnFailure procSpec err = case procSpec of
  WorkerSpec WorkerOptions { workerOnFailure } -> workerOnFailure err
  SupervisorSpec SupervisorOptions { supervisorOnFailure } ->
    supervisorOnFailure err

-- | Utility function to execute a Process onTermination sub-routine.
callProcessOnTermination :: ProcessSpec -> IO ()
callProcessOnTermination procSpec = case procSpec of
  WorkerSpec WorkerOptions { workerOnTermination } -> workerOnTermination
  _                                                -> return ()

-- | Handles errors produced - or thrown to - a process thread.
handleProcessException
  :: (IO () -> IO a)
  -> ParentSupervisorEnv
  -> ProcessSpec
  -> ProcessId
  -> RestartCount
  -> SomeException
  -> IO MonitorEvent
handleProcessException unmask ParentSupervisorEnv { supervisorId, supervisorName, notifyEvent } procSpec processId restartCount err
  = do
    let processName = getProcessName procSpec
    processThreadId  <- myThreadId
    monitorEventTime <- getCurrentTime
    case fromException err of
      Just RestartProcessException -> return ProcessForcedRestart
        { processId
        , processName
        , monitorEventTime
        }

      Just TerminateProcessException { processTerminationReason } -> do
        eErrResult <- try $ unmask $ callProcessOnTermination procSpec

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

      -- This exception was an error from the given sub-routine
      _ -> do
        eErrResult <- try $ unmask $ callProcessOnFailure procSpec err

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
  :: (IO () -> IO a)
  -> ParentSupervisorEnv
  -> ProcessSpec
  -> ProcessId
  -> RestartCount
  -> IO MonitorEvent
handleProcessCompletion unmask ParentSupervisorEnv { supervisorId, supervisorName, notifyEvent } procSpec processId restartCount
  = do
    let processName = getProcessName procSpec
    processThreadId  <- myThreadId
    monitorEventTime <- getCurrentTime
    eCompResult      <- try $ unmask $ callProcessOnCompletion procSpec

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
  :: Text -- ^ Description that indicates _why_ there is a termination
  -> SupervisorEnv
  -> Process
  -> IO ()
terminateProcess processTerminationReason env process = do
  case process of
    WorkerProcess worker -> terminateWorker processTerminationReason worker
    SupervisorProcess supervisor ->
      terminateSupervisor processTerminationReason supervisor

  notifyProcessTerminated env process processTerminationReason

-- | Internal utility function that manages execution of a termination policy
-- for a Worker.
terminateWorker :: Text -> Worker -> IO ()
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

      TimeoutMillis millis -> race_
        ( do
          threadDelay (millis * 1000)
          cancelWith
            workerAsync
            BrutallyTerminateProcessException
              { processId
              , processTerminationReason
              }
        )
        ( cancelWith
          workerAsync
          TerminateProcessException {processId , processTerminationReason }
        )

-- | Internal utility function that manages execution of a termination policy
-- for a Supervisor.
terminateSupervisor :: Text -> Supervisor -> IO ()
terminateSupervisor processTerminationReason Supervisor { supervisorId = processId, supervisorAsync }
  = cancelWith
    supervisorAsync
    TerminateProcessException {processId , processTerminationReason }

-- | Internal sub-routine that terminates workers of a supervisor, used when a
-- supervisor instance is terminated.
terminateProcessMap :: Text -> SupervisorEnv -> IO ()
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

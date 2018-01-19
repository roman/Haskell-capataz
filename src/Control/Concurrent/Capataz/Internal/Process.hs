{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}

-- | Process => Supervisor | Worker common code
module Control.Concurrent.Capataz.Internal.Process where

import Protolude

import Control.Concurrent.Capataz.Internal.Types
import Control.Concurrent.Capataz.Internal.Util
    (readProcessMap, sortProcessesByTerminationOrder)
import Data.Time.Clock                           (UTCTime, getCurrentTime)

-- | Fetch an "Async ()" from the Process thread
getProcessAsync :: Process -> Async ()
getProcessAsync process = case process of
  WorkerProcess     Worker { workerAsync }         -> workerAsync
  SupervisorProcess Supervisor { supervisorAsync } -> supervisorAsync

-- | Fetch the ThreadId of a Process thread
getProcessThreadId :: Process -> ThreadId
getProcessThreadId = asyncThreadId . getProcessAsync

-- | Fetch the ProcessId of a Process record
getProcessId :: Process -> ProcessId
getProcessId process = case process of
  WorkerProcess     Worker { workerId }         -> workerId
  SupervisorProcess Supervisor { supervisorId } -> supervisorId

-- | Fetch the ProcessName of a ProcessSpec record
getProcessName :: ProcessSpec -> ProcessName
getProcessName procSpec = case procSpec of
  WorkerSpec     WorkerOptions { workerName }         -> workerName
  SupervisorSpec SupervisorOptions { supervisorName } -> supervisorName

-- | Fetch the ProcessType of a ProcessSpec record
getProcessType :: ProcessSpec -> ProcessType
getProcessType processSpec = case processSpec of
  WorkerSpec{}     -> WorkerType
  SupervisorSpec{} -> SupervisorType

-- | Fetch the ProcessSpec of a Process record
getProcessSpec :: Process -> ProcessSpec
getProcessSpec process = case process of
  WorkerProcess Worker { workerOptions } -> WorkerSpec workerOptions
  SupervisorProcess Supervisor { supervisorOptions } ->
    SupervisorSpec supervisorOptions

-- | Utility function to send notifications when a Process fails
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

-- | Utility function to send notifications when a Process is terminated
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

-- | Utility function to send notifications when a Process gets started/restarted
notifyProcessStarted
  :: Maybe (ProcessId, RestartCount) -> ParentSupervisorEnv -> Process -> IO ()
notifyProcessStarted mRestartInfo ParentSupervisorEnv { supervisorId, supervisorName, notifyEvent } process
  = do
    eventTime <- getCurrentTime
    case mRestartInfo of
      Just (_workerId, processRestartCount) -> notifyEvent ProcessRestarted
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


-- | Utility function to send notifications when a Process completes
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


-- | Utility function to execution completion callback on a Process
callProcessOnCompletion :: ProcessSpec -> IO ()
callProcessOnCompletion procSpec = case procSpec of
  WorkerSpec WorkerOptions { workerOnCompletion } -> workerOnCompletion
  _                                               -> return ()

-- | Utility function to execution failure callback on a Process
callProcessOnFailure :: ProcessSpec -> SomeException -> IO ()
callProcessOnFailure procSpec err = case procSpec of
  WorkerSpec WorkerOptions { workerOnFailure } -> workerOnFailure err
  SupervisorSpec SupervisorOptions { supervisorOnFailure } ->
    supervisorOnFailure err

-- | Utility function to execution termination callback on a Process
callProcessOnTermination :: ProcessSpec -> IO ()
callProcessOnTermination procSpec = case procSpec of
  WorkerSpec WorkerOptions { workerOnTermination } -> workerOnTermination
  _                                                -> return ()

-- | Handles errors produced - or thrown to - Worker and Supervisor threads
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

-- | Handles completion of both Worker and Supervisor thread
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

-- | Utility functions to trigger termination of a Process
terminateProcess
  :: Text -- ^ Text that indicates why there is a termination
  -> SupervisorEnv
  -> Process
  -> IO ()
terminateProcess processTerminationReason env process = do
  case process of
    WorkerProcess worker -> terminateWorker processTerminationReason worker
    SupervisorProcess supervisor ->
      terminateSupervisor processTerminationReason supervisor

  notifyProcessTerminated env process processTerminationReason

-- | Internal function that manages execution of a termination policy for a
-- Worker
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

-- | Internal function that manages execution a Supervisor termination
terminateSupervisor :: Text -> Supervisor -> IO ()
terminateSupervisor processTerminationReason Supervisor { supervisorId = processId, supervisorAsync }
  = cancelWith
    supervisorAsync
    TerminateProcessException {processId , processTerminationReason }

-- | Internal sub-routine that terminates workers of a Supervisor, used when a
-- Supervisor instance is terminated
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

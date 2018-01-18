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

getProcessAsync :: Process -> Async ()
getProcessAsync process = case process of
  WorkerProcess     Worker { workerAsync }         -> workerAsync
  SupervisorProcess Supervisor { supervisorAsync } -> supervisorAsync

getProcessThreadId :: Process -> ThreadId
getProcessThreadId = asyncThreadId . getProcessAsync

getProcessId :: Process -> ProcessId
getProcessId process = case process of
  WorkerProcess     Worker { workerId }         -> workerId
  SupervisorProcess Supervisor { supervisorId } -> supervisorId

getProcessName :: ProcessSpec -> ProcessName
getProcessName procSpec = case procSpec of
  WorkerProcessSpec     WorkerSpec { workerName } -> workerName
  SupervisorProcessSpec SupervisorSpec { supervisorName } -> supervisorName

getProcessType :: ProcessSpec -> ProcessType
getProcessType processSpec = case processSpec of
  WorkerProcessSpec{}     -> WorkerType
  SupervisorProcessSpec{} -> SupervisorType

getProcessSpec :: Process -> ProcessSpec
getProcessSpec process = case process of
  WorkerProcess Worker { workerSpec } -> WorkerProcessSpec workerSpec
  SupervisorProcess Supervisor { supervisorSpec } ->
    SupervisorProcessSpec supervisorSpec

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

notifyProcessCompleted :: SupervisorEnv -> Process -> UTCTime -> IO ()
notifyProcessCompleted SupervisorEnv { supervisorId, supervisorName, notifyEvent } process eventTime
  =
    notifyEvent ProcessCompleted
  { supervisorId
  , supervisorName
  , processId       = getProcessId process
  , processName     = getProcessName (getProcessSpec process)
  , processType     = getProcessType (getProcessSpec process)
  , processThreadId = getProcessThreadId process
  , eventTime
  }


callProcessOnCompletion :: ProcessSpec -> IO ()
callProcessOnCompletion procSpec = case procSpec of
  WorkerProcessSpec WorkerSpec { workerOnCompletion } -> workerOnCompletion
  _                                                     -> return ()

callProcessOnFailure :: ProcessSpec -> SomeException -> IO ()
callProcessOnFailure procSpec err = case procSpec of
  WorkerProcessSpec WorkerSpec { workerOnFailure } -> workerOnFailure err
  SupervisorProcessSpec SupervisorSpec { supervisorOnFailure } ->
    supervisorOnFailure err

callProcessOnTermination :: ProcessSpec -> IO ()
callProcessOnTermination procSpec = case procSpec of
  WorkerProcessSpec WorkerSpec { workerOnTermination } -> workerOnTermination
  _                                                      -> return ()

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

-- | Handles errors caused by the execution of the "workerMain" sub-routine
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

-- | Handles completion of the "workerMain" sub-routine
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

terminateProcess
  :: Text -- ^ Text that indicates why there is a termination
  -> SupervisorEnv
  -> Process
  -> IO ()
terminateProcess processTerminationReason env process = case process of
  WorkerProcess worker -> terminateWorker processTerminationReason env worker
  SupervisorProcess supervisor ->
    terminateSupervisor processTerminationReason env supervisor

-- | Internal function that forks a worker thread on the Capataz thread; note
-- this is different from the public @forkWorker@ function which sends a message
-- to the capataz loop
terminateWorker :: Text -> SupervisorEnv -> Worker -> IO ()
terminateWorker processTerminationReason SupervisorEnv { supervisorId, supervisorName, notifyEvent } Worker { workerId, workerName, workerSpec, workerAsync }
  = do
    let processId                              = workerId
        processName                            = workerName
        WorkerSpec { workerTerminationPolicy } = workerSpec
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

    eventTime <- getCurrentTime
    notifyEvent ProcessTerminated
      { supervisorId
      , supervisorName
      , processId
      , processName
      , processType       = WorkerType
      , processThreadId   = asyncThreadId workerAsync
      , terminationReason = processTerminationReason
      , eventTime
      }

terminateSupervisor :: Text -> SupervisorEnv -> Supervisor -> IO ()
terminateSupervisor processTerminationReason SupervisorEnv { supervisorId, supervisorName, notifyEvent } Supervisor { supervisorId = processId, supervisorName = processName, supervisorAsync }
  = do

    cancelWith
      supervisorAsync
      TerminateProcessException {processId , processTerminationReason }

    eventTime <- getCurrentTime
    notifyEvent ProcessTerminated
      { supervisorId
      , supervisorName
      , eventTime
      , processId
      , processName
      , processType       = SupervisorType
      , processThreadId   = asyncThreadId supervisorAsync
      , terminationReason = processTerminationReason
      }

-- | Internal sub-routine that terminates workers of a Capataz, used when a
-- Capataz instance is terminated
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

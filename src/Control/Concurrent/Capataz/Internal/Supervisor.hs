{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Control.Concurrent.Capataz.Internal.Supervisor where

import Control.Concurrent.Async      (asyncWithUnmask)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar   (newTVarIO)
import Data.IORef                    (newIORef)
import Data.Time.Clock               (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Protolude

import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.V4        as UUID

import Control.Concurrent.Capataz.Internal.Types

import qualified Control.Concurrent.Capataz.Internal.Process as Process
import qualified Control.Concurrent.Capataz.Internal.Util    as Util
import qualified Control.Concurrent.Capataz.Internal.Worker  as Worker

-- | Internal function that forks a worker thread on the Supervisor thread; note
-- this is different from the public @forkSupervisor@ function which sends a message
-- to the supervisor loop
forkSupervisor
  :: ParentSupervisorEnv
  -> SupervisorOptions
  -> Maybe (ProcessId, RestartCount)
  -> IO Supervisor
forkSupervisor parentEnv supervisorOptions mRestartInfo = do
  (supervisorId, restartCount) <- case mRestartInfo of
    Just (supervisorId, restartCount) -> pure (supervisorId, restartCount)
    Nothing                           -> (,) <$> UUID.nextRandom <*> pure 0


  supervisor <- supervisorMain parentEnv
                               supervisorOptions
                               supervisorId
                               restartCount

  Process.notifyProcessStarted mRestartInfo
                               parentEnv
                               (SupervisorProcess supervisor)
  return supervisor

buildSupervisorEnv
  :: (CapatazEvent -> IO ())
  -> (SupervisorMessage -> IO ())
  -> STM SupervisorMessage
  -> SupervisorId
  -> SupervisorOptions
  -> IO SupervisorEnv
buildSupervisorEnv notifyEvent supervisorNotify supervisorGetNotification supervisorId supervisorOptions@SupervisorOptions {..}
  = do
    supervisorProcessMap <- newIORef mempty
    supervisorStatusVar  <- newTVarIO Initializing
    return SupervisorEnv {..}

-- | Handles an event produced by one of the workers this capataz monitors
handleMonitorEvent :: SupervisorEnv -> MonitorEvent -> IO Bool
handleMonitorEvent env monitorEv = do
  case monitorEv of
    ProcessForcedRestart{} ->
      -- We do nothing, as restart is being handled on restartWorkers
      -- sub-routine
      return ()

    ProcessCompleted' { processId, monitorEventTime } ->
      handleProcessCompleted env processId monitorEventTime

    ProcessFailed' { processId, processError, processRestartCount } ->
      handleProcessFailed env processId processError processRestartCount

    ProcessTerminated' { processId, processRestartCount, processTerminationReason }
      -> handleProcessTerminated env
                                 processId
                                 processTerminationReason
                                 processRestartCount


  return True

-- | Handles an action triggered by the public API
handleControlAction :: SupervisorEnv -> ControlAction -> IO Bool
handleControlAction env controlAction = case controlAction of
  ForkWorker { workerOptions, returnWorkerId } -> do
    worker@Worker { workerId } <- Worker.forkWorker env workerOptions Nothing
    Util.appendProcessToMap env (WorkerProcess worker)
    returnWorkerId workerId
    return True

  TerminateProcess { processId, processTerminationReason, notifyProcessTermination }
    -> do
      mProcess <- Util.fetchProcess env processId
      case mProcess of
        Just process -> do
          Process.terminateProcess processTerminationReason env process
          notifyProcessTermination
          return True
        _ -> return True

  TerminateCapataz { notifyCapatazTermination } -> do
    haltSupervisor "capataz termination" env
    notifyCapatazTermination
    return False

-- | Executes the shutdown operation of a Capataz, including the termination of
-- Workers being supervised by it.
haltSupervisor :: Text -> SupervisorEnv -> IO ()
haltSupervisor reason env = do
  Util.writeSupervisorStatus  env    Halting
  Process.terminateProcessMap reason env
  Util.resetProcessMap        env    (const HashMap.empty)
  Util.writeSupervisorStatus  env    Halted


-- | Handles all messages that a capataz instance can receive
handleSupervisorMessage :: SupervisorEnv -> SupervisorMessage -> IO Bool
handleSupervisorMessage env message = case message of
  ControlAction controlAction -> handleControlAction env controlAction
  MonitorEvent  monitorEvent  -> handleMonitorEvent env monitorEvent

-- | This is the main thread loop of a "Supervisor" instance
supervisorLoop
  :: (forall b . IO b -> IO b)
  -> ParentSupervisorEnv
  -> SupervisorEnv
  -> RestartCount
  -> IO ()
supervisorLoop unmask parentEnv@ParentSupervisorEnv { supervisorId, supervisorName, supervisorNotify = notifyParentSupervisor } env@SupervisorEnv { supervisorId = processId, supervisorName = processName, supervisorOptions, supervisorStatusVar, supervisorGetNotification, notifyEvent } restartCount
  = do
    processThreadId <- myThreadId
    loopResult      <-
      unmask
      $   try
      $   atomically
      $   (,)
      <$> Util.readCapatazStatusSTM supervisorStatusVar
      <*> supervisorGetNotification

    case loopResult of
      Left supervisorError -> do
        haltSupervisor (show supervisorError) env
        result <- Process.handleProcessException
          unmask
          parentEnv
          (SupervisorSpec supervisorOptions)
          processId
          restartCount
          supervisorError
        notifyParentSupervisor (MonitorEvent result)

      Right (status, message) -> case status of
        Initializing -> do
          eventTime <- getCurrentTime
          notifyEvent InvalidCapatazStatusReached
            { supervisorId
            , supervisorName
            , eventTime
            }
          supervisorLoop unmask parentEnv env restartCount

        Running -> do
          eContinueLoop <- try $ unmask $ handleSupervisorMessage env message
          case eContinueLoop of
            Left supervisorError -> do
              haltSupervisor (show supervisorError) env
              result <- Process.handleProcessException
                unmask
                parentEnv
                (SupervisorSpec supervisorOptions)
                processId
                restartCount
                supervisorError
              notifyParentSupervisor (MonitorEvent result)

            Right continueLoop
              | continueLoop -> supervisorLoop unmask parentEnv env restartCount
              | otherwise -> do
                eventTime <- getCurrentTime
                notifyEvent ProcessTerminated
                  { supervisorId
                  , supervisorName
                  , eventTime
                  , processId
                  , processName
                  , processThreadId
                  , processType       = SupervisorType
                  , terminationReason = "Supervisor normal termination"
                  }

        Halting ->
          -- Discard messages when halting
          return ()

        Halted ->
          -- Discard messages when halted
          return ()


-- | Decorates the given @IO ()@ sub-routine with failure handling
supervisorMain
  :: ParentSupervisorEnv
  -> SupervisorOptions
  -> SupervisorId
  -> RestartCount
  -> IO Supervisor
supervisorMain parentEnv@ParentSupervisorEnv { notifyEvent } supervisorOptions@SupervisorOptions { supervisorName, supervisorProcessSpecList } supervisorId restartCount
  = do
    supervisorCreationTime <- getCurrentTime

    supervisorQueue        <- newTQueueIO
    let supervisorNotify          = atomically . writeTQueue supervisorQueue
        supervisorGetNotification = readTQueue supervisorQueue

    supervisorEnv@SupervisorEnv{} <- buildSupervisorEnv
      notifyEvent
      supervisorNotify
      supervisorGetNotification
      supervisorId
      supervisorOptions

    supervisorAsync <- asyncWithUnmask $ \unmask -> do
      Util.setProcessThreadName supervisorId supervisorName
      supervisorLoop unmask parentEnv supervisorEnv restartCount

    forM_
      supervisorProcessSpecList
      ( \processSpec -> case processSpec of
        WorkerSpec workerOptions -> do
          worker <- Worker.forkWorker supervisorEnv workerOptions Nothing
          Util.appendProcessToMap supervisorEnv (WorkerProcess worker)

        SupervisorSpec childSupervisorOptions -> do
          supervisor <- forkSupervisor
            (Util.toParentSupervisorEnv supervisorEnv)
            childSupervisorOptions
            Nothing
          Util.appendProcessToMap supervisorEnv (SupervisorProcess supervisor)
      )

    Util.writeSupervisorStatus supervisorEnv Running

    return Supervisor
      { supervisorId
      , supervisorName
      , supervisorAsync
      , supervisorOptions
      , supervisorEnv
      , supervisorNotify
      , supervisorCreationTime
      }


--------------------------------------------------------------------------------

-- | Function used to track difference between two timestamps to track capataz
-- error intensity
calcDiffSeconds :: UTCTime -> IO NominalDiffTime
calcDiffSeconds creationTime = do
  currentTime <- getCurrentTime
  return $ diffUTCTime currentTime creationTime

-- | Function that checks restart counts and worker start time to assess if the
-- capataz error intensity has been breached, see "WorkerRestartAction" for
-- possible outcomes.
calcRestartAction
  :: SupervisorEnv -> Int -> NominalDiffTime -> WorkerRestartAction
calcRestartAction SupervisorEnv { supervisorIntensity, supervisorPeriodSeconds } restartCount diffSeconds
  = case () of
    _
      | diffSeconds
        <  supervisorPeriodSeconds
        && restartCount
        >= supervisorIntensity
      -> HaltSupervisor
      | diffSeconds > supervisorPeriodSeconds
      -> ResetRestartCount
      | otherwise
      -> IncreaseRestartCount

-- | Sub-routine responsible of executing a "SupervisorRestartStrategy"
execCapatazRestartStrategy
  :: SupervisorEnv -> ProcessId -> ProcessSpec -> Int -> IO ()
execCapatazRestartStrategy supervisorEnv@SupervisorEnv { supervisorRestartStrategy } processId processSpec processRestartCount
  = case supervisorRestartStrategy of
    AllForOne -> do
      newProcessList <- restartProcessList supervisorEnv
                                           processId
                                           processRestartCount
      let newProcessMap =
            newProcessList
              & fmap (\process -> (Util.getProcessId process, process))
              & HashMap.fromList
      Util.resetProcessMap supervisorEnv (const newProcessMap)

    OneForOne -> do
      Util.removeProcessFromMap supervisorEnv processId
      newProcess <- case processSpec of
        WorkerSpec workerOptions -> restartWorker supervisorEnv
                                                  workerOptions
                                                  processId
                                                  processRestartCount

        SupervisorSpec supervisorOptions -> restartSupervisor
          (Util.toParentSupervisorEnv supervisorEnv)
          supervisorOptions
          processId
          processRestartCount

      Util.appendProcessToMap supervisorEnv newProcess

-- | Executes a restart action returned from the invokation of "calcRestartAction"
execRestartAction
  :: SupervisorEnv
  -> ProcessId
  -> ProcessSpec
  -> Text
  -> UTCTime
  -> Int
  -> IO ()
execRestartAction supervisorEnv@SupervisorEnv { supervisorOnIntensityReached } processId processSpec processName processCreationTime processRestartCount
  = do
    restartAction <- calcRestartAction supervisorEnv processRestartCount
      <$> calcDiffSeconds processCreationTime

    case restartAction of
      HaltSupervisor -> do
        -- skip exceptions on callback
        (_ :: Either SomeException ()) <- try supervisorOnIntensityReached
        throwIO SupervisorIntensityReached
          { processId
          , processName
          , processRestartCount
          }

      ResetRestartCount ->
        execCapatazRestartStrategy supervisorEnv processId processSpec 0

      IncreaseRestartCount -> execCapatazRestartStrategy
        supervisorEnv
        processId
        processSpec
        (succ processRestartCount)


--------------------------------------------------------------------------------

-- | Restarts _all_ the worker green thread of a Capataz, invoked when one
-- worker green thread fails and causes sibling worker threads to get restarted
-- as well
restartProcessList :: SupervisorEnv -> WorkerId -> RestartCount -> IO [Process]
restartProcessList supervisorEnv@SupervisorEnv { supervisorProcessTerminationOrder } failingProcessId restartCount
  = do
    processMap <- Util.readProcessMap supervisorEnv

    let processList = Util.sortProcessesByTerminationOrder
          supervisorProcessTerminationOrder
          processMap

    newProcessList <- forM processList $ \process -> do
      unless (failingProcessId == Process.getProcessId process)
        $ forceRestartProcess supervisorEnv process

      case process of
        WorkerProcess Worker { workerId, workerOptions } -> do
          let WorkerOptions { workerRestartStrategy } = workerOptions
          case workerRestartStrategy of
            Temporary -> return Nothing
            _ ->
              Just
                <$> restartWorker supervisorEnv
                                  workerOptions
                                  workerId
                                  restartCount

        SupervisorProcess Supervisor { supervisorId, supervisorOptions } ->
          Just
            <$> restartSupervisor (Util.toParentSupervisorEnv supervisorEnv)
                                  supervisorOptions
                                  supervisorId
                                  restartCount


    return $ catMaybes newProcessList

-- | Sub-routine that is used when there is a restart request to a Worker caused
-- by an "AllForOne" restart from a failing sibling worker.
forceRestartProcess :: SupervisorEnv -> Process -> IO ()
forceRestartProcess env process = do
  Process.notifyProcessTerminated env process "forced restart"
  cancelWith (Process.getProcessAsync process) RestartProcessException

-- | Starts a new worker thread taking into account an existing "WorkerId" and
-- keeping a "RestartCount" to manage Capataz error intensity.
restartWorker
  :: SupervisorEnv -> WorkerOptions -> WorkerId -> RestartCount -> IO Process
restartWorker supervisorEnv workerOptions workerId restartCount =
  WorkerProcess <$> Worker.forkWorker supervisorEnv
                                      workerOptions
                                      (Just (workerId, restartCount))

restartSupervisor
  :: ParentSupervisorEnv
  -> SupervisorOptions
  -> ProcessId
  -> RestartCount
  -> IO Process
restartSupervisor parentEnv supervisorOptions processId restartCount =
  SupervisorProcess <$> forkSupervisor parentEnv
                                       supervisorOptions
                                       (Just (processId, restartCount))


--------------------------------------------------------------------------------

handleWorkerCompleted :: SupervisorEnv -> Worker -> IO ()
handleWorkerCompleted env worker = do
  let Worker { workerId, workerOptions, workerCreationTime } = worker
      WorkerOptions { workerName, workerRestartStrategy }    = workerOptions
  case workerRestartStrategy of
    Permanent -> do
      -- NOTE: Completed workers should never account as errors happening on
      -- a supervised thread, ergo, they should be restarted every time.

      -- TODO: Notify a warning around having a workerRestartStrategy different
      -- than Temporary on workers that may complete.
      let restartCount = 0
      execRestartAction env
                        workerId
                        (WorkerSpec workerOptions)
                        workerName
                        workerCreationTime
                        restartCount

    _ -> Util.removeProcessFromMap env workerId

-- | This sub-routine is responsible of the restart strategies execution when a
-- supervised worker finishes it execution because of a completion (e.g. worker
-- sub-routine finished without any errors).
handleProcessCompleted :: SupervisorEnv -> ProcessId -> UTCTime -> IO ()
handleProcessCompleted env processId completionTime = do
  mProcess <- Util.fetchProcess env processId
  case mProcess of
    Nothing      -> return ()

    Just process -> do
      Process.notifyProcessCompleted env process completionTime
      case process of
        WorkerProcess worker -> handleWorkerCompleted env worker
        _ ->
          panic
            $  "ERROR: Supervisor ("
            <> show (Process.getProcessId process)
            <> ") should never complete"

handleWorkerFailed :: SupervisorEnv -> Worker -> Int -> IO ()
handleWorkerFailed env worker restartCount = do
  let Worker { workerId, workerCreationTime, workerOptions } = worker
      WorkerOptions { workerName, workerRestartStrategy }    = workerOptions
  case workerRestartStrategy of
    Temporary -> Util.removeProcessFromMap env workerId
    _         -> execRestartAction env
                                   workerId
                                   (WorkerSpec workerOptions)
                                   workerName
                                   workerCreationTime
                                   restartCount

handleSupervisorFailed :: SupervisorEnv -> Supervisor -> Int -> IO ()
handleSupervisorFailed env supervisor restartCount = do
  let Supervisor { supervisorId, supervisorCreationTime, supervisorOptions } =
        supervisor
      SupervisorOptions { supervisorName } = supervisorOptions
  execRestartAction env
                    supervisorId
                    (SupervisorSpec supervisorOptions)
                    supervisorName
                    supervisorCreationTime
                    restartCount

-- | This sub-routine is responsible of the restart strategies execution when a
-- supervised worker finishes it execution because of a failure.
handleProcessFailed
  :: SupervisorEnv -> WorkerId -> SomeException -> Int -> IO ()
handleProcessFailed env processId processError restartCount = do
  mProcess <- Util.fetchProcess env processId
  case mProcess of
    Nothing      -> return ()
    Just process -> do
      Process.notifyProcessFailed env process processError
      case process of
        WorkerProcess worker -> handleWorkerFailed env worker restartCount

        SupervisorProcess supervisor ->
          handleSupervisorFailed env supervisor restartCount

handleWorkerTerminated :: SupervisorEnv -> Worker -> Int -> IO ()
handleWorkerTerminated env worker restartCount = do
  let Worker { workerId, workerCreationTime, workerOptions } = worker
      WorkerOptions { workerName, workerRestartStrategy }    = workerOptions

  case workerRestartStrategy of
    Permanent -> execRestartAction env
                                   workerId
                                   (WorkerSpec workerOptions)
                                   workerName
                                   workerCreationTime
                                   restartCount

    _ -> Util.removeProcessFromMap env workerId

handleSupervisorTerminated :: SupervisorEnv -> Supervisor -> Int -> IO ()
handleSupervisorTerminated env supervisor restartCount = do
  let Supervisor { supervisorId, supervisorCreationTime, supervisorOptions } =
        supervisor
      SupervisorOptions { supervisorName } = supervisorOptions
  execRestartAction env
                    supervisorId
                    (SupervisorSpec supervisorOptions)
                    supervisorName
                    supervisorCreationTime
                    restartCount

-- | This sub-routine is responsible of the restart strategies execution when a
-- supervised worker finishes it execution because of a termination.
handleProcessTerminated :: SupervisorEnv -> ProcessId -> Text -> Int -> IO ()
handleProcessTerminated env processId terminationReason restartCount = do
  mProcess <- Util.fetchProcess env processId
  case mProcess of
    Nothing      -> return ()
    Just process -> do
      Process.notifyProcessTerminated env process terminationReason
      case process of
        WorkerProcess worker -> handleWorkerTerminated env worker restartCount

        SupervisorProcess supervisor ->
          handleSupervisorTerminated env supervisor restartCount

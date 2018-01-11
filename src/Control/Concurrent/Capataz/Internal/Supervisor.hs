{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
module Control.Concurrent.Capataz.Internal.Supervisor where

import           Control.Concurrent.Async      (asyncWithUnmask)
import           Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import           Control.Concurrent.STM.TVar   (newTVarIO)
import qualified Data.HashMap.Strict           as HashMap
import           Data.IORef                    (newIORef)
import           Data.Time.Clock               (getCurrentTime)
import qualified Data.UUID.V4                  as UUID
import           Protolude

import Control.Concurrent.Capataz.Internal.Types

import qualified Control.Concurrent.Capataz.Internal.Process as Process
import qualified Control.Concurrent.Capataz.Internal.Restart as Restart
import qualified Control.Concurrent.Capataz.Internal.Util    as Util
import qualified Control.Concurrent.Capataz.Internal.Worker  as Worker

-- | Internal function that forks a worker thread on the Supervisor thread; note
-- this is different from the public @forkSupervisor@ function which sends a message
-- to the supervisor loop
forkSupervisor
  :: ParentSupervisorEnv
  -> SupervisorSpec
  -> Maybe (ProcessId, RestartCount)
  -> IO Supervisor
forkSupervisor parentEnv supervisorSpec mRestartInfo = do
  (supervisorId, restartCount) <- case mRestartInfo of
    Just (supervisorId, restartCount) -> pure (supervisorId, restartCount)
    Nothing                           -> (,) <$> UUID.nextRandom <*> pure 0


  supervisor <- supervisorMain parentEnv
                               supervisorSpec
                               supervisorId
                               restartCount

  Process.notifyProcessStarted mRestartInfo
                               parentEnv
                               (SupervisorProcess supervisor)
  return supervisor

buildSupervisorEnv
  :: (CapatazEvent -> IO ())
  -> (SupervisorMessage -> IO ())
  -> (STM SupervisorMessage)
  -> SupervisorId
  -> SupervisorSpec
  -> IO SupervisorEnv
buildSupervisorEnv notifyEvent supervisorNotify supervisorGetNotification supervisorId supervisorSpec@SupervisorSpec {..}
  = do
    supervisorProcessMap <- newIORef mempty
    supervisorStatusVar  <- newTVarIO Initializing
    return $ SupervisorEnv {..}

-- | Handles an event produced by one of the workers this capataz monitors
handleMonitorEvent :: SupervisorEnv -> MonitorEvent -> IO Bool
handleMonitorEvent env monitorEv = do
  case monitorEv of
    ProcessForcedRestart{} ->
      -- We do nothing, as restart is being handled on restartWorkers
      -- sub-routine
      return ()

    ProcessCompleted' { processId, monitorEventTime } ->
      Restart.handleWorkerCompleted env processId monitorEventTime

    ProcessFailed' { processId, processError, processRestartCount } ->
      Restart.handleWorkerFailed env processId processError processRestartCount

    ProcessTerminated' { processId, processRestartCount, processTerminationReason }
      -> Restart.handleWorkerTerminated env
                                        processId
                                        processTerminationReason
                                        processRestartCount


  return True

-- | Handles an action triggered by the public API
handleControlAction :: SupervisorEnv -> ControlAction -> IO Bool
handleControlAction env controlAction = case controlAction of
  ForkWorker { workerSpec, returnWorkerId } -> do
    worker@Worker { workerId } <- Worker.forkWorker env workerSpec Nothing
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
  Util.writeSupervisorStatus  env                   Halting
  Process.terminateProcessMap reason env
  Util.resetProcessMap        env                   (const HashMap.empty)
  Util.writeSupervisorStatus  env                   Halted


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
supervisorLoop unmask parentEnv@ParentSupervisorEnv { supervisorId, supervisorName, supervisorNotify = notifyParentSupervisor } env@SupervisorEnv { supervisorId = processId, supervisorName = processName, supervisorSpec, supervisorStatusVar, supervisorGetNotification, notifyEvent } restartCount
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
          (SupervisorProcessSpec supervisorSpec)
          supervisorId
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
                (SupervisorProcessSpec supervisorSpec)
                supervisorId
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
  -> SupervisorSpec
  -> SupervisorId
  -> RestartCount
  -> IO Supervisor
supervisorMain parentEnv@ParentSupervisorEnv { notifyEvent } supervisorSpec@SupervisorSpec { supervisorName, supervisorProcessSpecList } supervisorId restartCount
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
      supervisorSpec

    supervisorAsync <- asyncWithUnmask $ \unmask -> do
      Util.setProcessThreadName supervisorId supervisorName
      supervisorLoop unmask parentEnv supervisorEnv restartCount

    forM_
      supervisorProcessSpecList
      ( \processSpec -> case processSpec of
        WorkerProcessSpec workerSpec -> do
          worker <- Worker.forkWorker supervisorEnv workerSpec Nothing
          Util.appendProcessToMap supervisorEnv (WorkerProcess worker)

        SupervisorProcessSpec childSupervisorSpec -> do
          supervisor <- forkSupervisor
            (Util.toParentSupervisorEnv supervisorEnv)
            childSupervisorSpec
            Nothing
          Util.appendProcessToMap supervisorEnv (SupervisorProcess supervisor)
      )

    Util.writeSupervisorStatus supervisorEnv Running

    return Supervisor
      { supervisorId
      , supervisorName
      , supervisorAsync
      , supervisorSpec
      , supervisorEnv
      , supervisorNotify
      , supervisorCreationTime
      }

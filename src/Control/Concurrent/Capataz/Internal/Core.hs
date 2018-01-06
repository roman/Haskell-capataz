{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-| This module contains:

* Functions exported on the public API
* The supervisor thread loop
* High level message handlers of the supervisor thread loop

-}
module Control.Concurrent.Capataz.Internal.Core where

import Protolude

import Control.Concurrent.Async      (asyncWithUnmask)
import Control.Concurrent.MVar       (newEmptyMVar, takeMVar)
import Control.Concurrent.STM        (atomically)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar   (newTVarIO)
import Control.Teardown              (newTeardown)
import Data.IORef                    (newIORef)
import Data.Time.Clock               (getCurrentTime)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.V4        as UUID (nextRandom)

import qualified Control.Concurrent.Capataz.Internal.Restart as Restart
import qualified Control.Concurrent.Capataz.Internal.Worker  as Worker

import Control.Concurrent.Capataz.Internal.Types
import Control.Concurrent.Capataz.Internal.Util
    ( appendProcessToMap
    , capatazToEnv
    , fetchProcess
    , readCapatazStatus
    , readCapatazStatusSTM
    , resetProcessMap
    , sendSyncControlMsg
    , workerOptionsToSpec
    , writeCapatazStatus
    )

--------------------------------------------------------------------------------

-- | Executes the shutdown operation of a Capataz, including the termination of
-- Workers being supervised by it.
haltCapataz :: CapatazEnv -> IO ()
haltCapataz env = do
  writeCapatazStatus      env                Halting
  Worker.terminateWorkers "capataz shutdown" env
  resetProcessMap         env                (const HashMap.empty)
  writeCapatazStatus      env                Halted

-- | Handles an event produced by one of the workers this capataz monitors
handleMonitorEvent :: CapatazEnv -> MonitorEvent -> IO Bool
handleMonitorEvent env monitorEv = do
  case monitorEv of
    WorkerForcedRestart{} ->
      -- We do nothing, as restart is being handled on restartWorkers
      -- sub-routine
      return ()

    WorkerCompleted' { workerId, monitorEventTime } ->
      Restart.handleWorkerCompleted env workerId monitorEventTime

    WorkerFailed' { workerId, workerError, workerRestartCount } ->
      Restart.handleWorkerFailed env workerId workerError workerRestartCount

    WorkerTerminated' { workerId, workerRestartCount, workerTerminationReason }
      -> Restart.handleWorkerTerminated env
                                        workerId
                                        workerTerminationReason
                                        workerRestartCount


  return True

-- | Handles an action triggered by the public API
handleControlAction :: CapatazEnv -> ControlAction -> IO Bool
handleControlAction env controlAction = case controlAction of
  ForkWorker { workerSpec, returnWorkerId } -> do
    worker@Worker { workerId } <- Worker.forkWorker env workerSpec Nothing
    appendProcessToMap env (WorkerProcess worker)
    returnWorkerId workerId
    return True

  TerminateWorker { terminationReason, workerId, notifyWorkerTermination } ->
    do
      mProcess <- fetchProcess env workerId
      case mProcess of
        Just (WorkerProcess worker) -> do
          Worker.terminateWorker terminationReason env worker
          notifyWorkerTermination
          return True
        _ -> return True

  TerminateCapataz { notifyCapatazTermination } -> do
    haltCapataz env
    notifyCapatazTermination
    return False

-- | Handles all messages that a capataz instance can receive
handleCapatazMessage :: CapatazEnv -> CapatazMessage -> IO Bool
handleCapatazMessage env message = case message of
  ControlAction controlAction -> handleControlAction env controlAction
  MonitorEvent  monitorEvent  -> handleMonitorEvent env monitorEvent

-- | Handles errors caused by the execution of the "runCapatazLoop" sub-routine
handleCapatazException :: CapatazEnv -> SomeException -> IO ()
handleCapatazException env@CapatazEnv { capatazId, capatazName, notifyEvent } capatazError
  = do
    eventTime <- getCurrentTime
    notifyEvent CapatazFailed
      { capatazId
      , capatazName
      , capatazError
      , eventTime
      }
    haltCapataz env
    throwIO capatazError

-- | This is the main thread loop of a "Capataz" instance
runCapatazLoop :: (forall b . IO b -> IO b) -> CapatazEnv -> IO ()
runCapatazLoop unmask env@CapatazEnv { capatazId, capatazName, capatazStatusVar, capatazQueue, notifyEvent }
  = do
    loopResult <-
      unmask
      $   try
      $   atomically
      $   (,)
      <$> readCapatazStatusSTM capatazStatusVar
      <*> readTQueue capatazQueue

    case loopResult of
      Left  capatazError      -> handleCapatazException env capatazError

      Right (status, message) -> case status of
        Initializing -> do
          eventTime <- getCurrentTime
          notifyEvent InvalidCapatazStatusReached
            { capatazId
            , capatazName
            , eventTime
            }
          runCapatazLoop unmask env

        Running -> do
          eContinueLoop <- try $ unmask $ handleCapatazMessage env message
          case eContinueLoop of
            Left capatazError -> handleCapatazException env capatazError

            Right continueLoop
              | continueLoop -> runCapatazLoop unmask env
              | otherwise -> do
                eventTime <- getCurrentTime
                notifyEvent CapatazTerminated
                  { capatazId
                  , capatazName
                  , eventTime
                  }

        Halting ->
          -- Discard messages when halting
          return ()

        Halted -> panic "TODO: Pending halted state"

-- | Builds a record that contains runtime values of a "Capataz" (id, queue, status, etc.)
buildCapatazRuntime :: CapatazOptions -> IO CapatazRuntime
buildCapatazRuntime capatazOptions = do
  capatazId           <- UUID.nextRandom
  capatazCreationTime <- getCurrentTime
  capatazQueue        <- newTQueueIO
  capatazStatusVar    <- newTVarIO Initializing
  capatazProcessMap   <- newIORef HashMap.empty
  return CapatazRuntime {..}

-- | Creates a Capataz record, which represents a supervision thread which
-- monitors failure on worker threads defined in the "CapatazOptions" or worker
-- threads that are created dynamically using "forkWorker".
forkCapataz :: CapatazOptions -> IO Capataz
forkCapataz capatazOptions@CapatazOptions { capatazName, capatazProcessSpecList, notifyEvent }
  = do
    capatazRuntime <- buildCapatazRuntime capatazOptions

    let capatazEnv@CapatazEnv { capatazId } = capatazToEnv capatazRuntime

    capatazAsync <- asyncWithUnmask
      $ \unmask -> runCapatazLoop unmask capatazEnv

    forM_
      capatazProcessSpecList
      ( \processSpec -> case processSpec of
        WorkerProcessSpec workerSpec -> do
          worker <- Worker.forkWorker capatazEnv workerSpec Nothing
          appendProcessToMap capatazEnv (WorkerProcess worker)

        SupervisorProcessSpec _capatazOptions -> panic "pending"
      )

    writeCapatazStatus capatazEnv Running

    capatazTeardown <- newTeardown
      ("capataz[" <> capatazName <> "]")
      ( do
        status <- readCapatazStatus capatazEnv
        case status of
          Halted  -> return ()
          Halting -> return ()
          _       -> do
            eventTime <- getCurrentTime
            notifyEvent CapatazShutdownInvoked
              { capatazId
              , capatazName
              , eventTime
              }
            sendSyncControlMsg capatazEnv TerminateCapataz
      )

    return Capataz {..}

-- | Creates a worker green thread "IO ()" sub-routine, and depending in options
-- defined in the "WorkerOptions" record, it will restart the Worker sub-routine
-- in case of failures
forkWorker
  :: WorkerOptions -- ^ Worker options (restart, name, callbacks, etc)
  -> IO ()         -- ^ IO sub-routine that will be executed on worker thread
  -> Capataz       -- ^ "Capataz" instance that supervises the worker
  -> IO WorkerId   -- ^ An identifier that can be used to terminate the "Worker"
forkWorker workerOptions workerAction Capataz { capatazEnv } = do
  let workerSpec = workerOptionsToSpec workerOptions workerAction
      CapatazEnv { capatazQueue } = capatazEnv

  workerIdVar <- newEmptyMVar
  atomically $ writeTQueue
    capatazQueue
    ( ControlAction ForkWorker
      { workerSpec
      , returnWorkerId = putMVar workerIdVar
      }
    )
  takeMVar workerIdVar

-- | Stops the execution of a worker green thread being supervised by the given
-- "Capataz" instance, if the WorkerId does not belong to the Capataz, the
-- operation does not perform any side-effect.
--
-- Note: If your worker has a "Permanent" worker restart strategy, the worker
-- thread __will be restarted again__; so use a "Transient" restart strategy
-- instead.
terminateWorker :: Text -> WorkerId -> Capataz -> IO ()
terminateWorker terminationReason workerId Capataz { capatazEnv } =
  sendSyncControlMsg
    capatazEnv
    ( \notifyWorkerTermination -> TerminateWorker
      { terminationReason
      , workerId
      , notifyWorkerTermination
      }
    )

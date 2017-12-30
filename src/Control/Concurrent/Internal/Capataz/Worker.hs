{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Control.Concurrent.Internal.Capataz.Worker where

import Protolude

import Control.Concurrent.Async      (asyncWithUnmask)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Data.Time.Clock               (getCurrentTime)
import GHC.Conc                      (labelThread)

import qualified Data.Text    as T
import qualified Data.UUID.V4 as UUID

import Control.Concurrent.Internal.Capataz.Types
import Control.Concurrent.Internal.Capataz.Util
    (getTidNumber, readWorkerMap, sortWorkersByTerminationOrder)

-- | Internal functions that overwrites the GHC thread name, for increasing
-- traceability on GHC internals
setWorkerThreadName :: WorkerId -> WorkerName -> IO ()
setWorkerThreadName workerId workerName = do
  tid <- myThreadId
  let workerIdentifier =
        T.unpack workerName <> "_" <> show workerId <> "_" <> maybe
          ""
          T.unpack
          (getTidNumber tid)
  labelThread tid workerIdentifier

-- | Handles errors caused by the execution of the "workerMain" sub-routine
handleWorkerException
  :: (IO () -> IO a)
  -> CapatazEnv
  -> WorkerSpec
  -> WorkerId
  -> RestartCount
  -> SomeException
  -> IO MonitorEvent
handleWorkerException unmask CapatazEnv { capatazId, capatazName, notifyEvent } WorkerSpec { workerName, workerOnFailure, workerOnTermination } workerId restartCount err
  = do
    workerThreadId   <- myThreadId
    monitorEventTime <- getCurrentTime
    case fromException err of
      Just RestartWorkerException ->
        return WorkerForcedRestart {workerId , workerName , monitorEventTime }

      Just TerminateWorkerException { workerTerminationReason } -> do
        eErrResult <- try $ unmask workerOnTermination

        notifyEvent WorkerCallbackExecuted
          { capatazId
          , capatazName
          , workerId
          , workerName
          , workerThreadId
          , workerCallbackError = either Just (const Nothing) eErrResult
          , callbackType        = OnTermination
          , eventTime           = monitorEventTime
          }

        case eErrResult of
          Left workerCallbackError -> return WorkerFailed'
            { workerName
            , workerId
            , monitorEventTime
            , workerError        = toException WorkerCallbackFailed
              { workerId
              , workerCallbackError
              , callbackType        = OnTermination
              , workerActionError   = Just err
              }
            , workerRestartCount = restartCount
            }
          Right _ -> return WorkerTerminated'
            { workerId
            , workerName
            , monitorEventTime
            , workerTerminationReason
            , workerRestartCount      = restartCount
            }

      Just BrutallyTerminateWorkerException { workerTerminationReason } ->
        return WorkerTerminated'
          { workerId
          , workerName
          , monitorEventTime
          , workerTerminationReason
          , workerRestartCount      = restartCount
          }

      -- This exception was an error from the given sub-routine
      Nothing -> do
        eErrResult <- try $ unmask $ workerOnFailure err

        notifyEvent WorkerCallbackExecuted
          { capatazId
          , capatazName
          , workerId
          , workerName
          , workerThreadId
          , workerCallbackError = either Just (const Nothing) eErrResult
          , callbackType        = OnFailure
          , eventTime           = monitorEventTime
          }

        case eErrResult of
          Left workerCallbackError -> return WorkerFailed'
            { workerName
            , workerId
            , monitorEventTime
            , workerRestartCount = restartCount
            , workerError        = toException WorkerCallbackFailed
              { workerId
              , workerCallbackError
              , callbackType        = OnFailure
              , workerActionError   = Just err
              }
            }
          Right _ -> return WorkerFailed'
            { workerName
            , workerId
            , monitorEventTime
            , workerError        = err
            , workerRestartCount = restartCount
            }

-- | Handles completion of the "workerMain" sub-routine
handleWorkerCompletion
  :: (IO () -> IO a)
  -> CapatazEnv
  -> WorkerSpec
  -> WorkerId
  -> RestartCount
  -> IO MonitorEvent
handleWorkerCompletion unmask CapatazEnv { capatazId, capatazName, notifyEvent } WorkerSpec { workerName, workerOnCompletion } workerId restartCount
  = do
    workerThreadId   <- myThreadId
    monitorEventTime <- getCurrentTime
    eCompResult      <- try $ unmask workerOnCompletion

    notifyEvent WorkerCallbackExecuted
      { capatazId
      , capatazName
      , workerId
      , workerName
      , workerThreadId
      , workerCallbackError = either Just (const Nothing) eCompResult
      , callbackType        = OnCompletion
      , eventTime           = monitorEventTime
      }

    case eCompResult of
      Left err -> return WorkerFailed'
        { workerName
        , workerId
        , monitorEventTime
        , workerError        = toException WorkerCallbackFailed
          { workerId
          , workerCallbackError = err
          , callbackType        = OnCompletion
          , workerActionError   = Nothing
          }
        , workerRestartCount = restartCount
        }
      Right _ ->
        return WorkerCompleted' {workerName , workerId , monitorEventTime }

-- | Decorates the given @IO ()@ sub-routine with failure handling
workerMain :: CapatazEnv -> WorkerSpec -> WorkerId -> RestartCount -> IO Worker
workerMain env@CapatazEnv { capatazQueue } workerSpec@WorkerSpec { workerName, workerAction } workerId restartCount
  = do
    workerCreationTime <- getCurrentTime
    workerAsync        <- asyncWithUnmask $ \unmask -> do

      eResult <- try $ do
        setWorkerThreadName workerId workerName
        unmask workerAction

      resultEvent <- case eResult of
        Left err ->
          handleWorkerException unmask env workerSpec workerId restartCount err
        Right _ ->
          handleWorkerCompletion unmask env workerSpec workerId restartCount

      atomically $ writeTQueue capatazQueue (MonitorEvent resultEvent)

    return Worker
      { workerId
      , workerName
      , workerAsync
      , workerCreationTime
      , workerSpec
      }

-- | Internal function used to send a proper "CapatazEvent" to the "notifyEvent"
-- callback, this event can either be a @WorkerStarted@ or a @WorkerRestarted@
notifyWorkerStarted :: Maybe (WorkerId, Int) -> CapatazEnv -> Worker -> IO ()
notifyWorkerStarted mRestartInfo CapatazEnv { capatazId, capatazName, notifyEvent } Worker { workerId, workerName, workerAsync }
  = do
    eventTime <- getCurrentTime
    case mRestartInfo of
      Just (_workerId, workerRestartCount) -> notifyEvent WorkerRestarted
        { capatazId
        , capatazName
        , workerId
        , workerName
        , workerRestartCount
        , workerThreadId     = asyncThreadId workerAsync
        , eventTime
        }
      Nothing -> notifyEvent WorkerStarted
        { capatazId
        , capatazName
        , workerId
        , workerName
        , eventTime
        , workerThreadId = asyncThreadId workerAsync
        }

-- | Internal function that forks a worker thread on the Capataz thread; note
-- this is different from the public @forkWorker@ function which sends a message
-- to the capataz loop
forkWorker
  :: CapatazEnv -> WorkerSpec -> Maybe (WorkerId, RestartCount) -> IO Worker
forkWorker env workerSpec mRestartInfo = do
  (workerId, restartCount) <- case mRestartInfo of
    Just (workerId, restartCount) -> pure (workerId, restartCount)
    Nothing                       -> (,) <$> UUID.nextRandom <*> pure 0

  worker <- workerMain env workerSpec workerId restartCount
  notifyWorkerStarted mRestartInfo env worker
  return worker

-- | Internal function that forks a worker thread on the Capataz thread; note
-- this is different from the public @forkWorker@ function which sends a message
-- to the capataz loop
terminateWorker
  :: Text -- ^ Text that indicates why there is a termination
  -> CapatazEnv
  -> Worker
  -> IO ()
terminateWorker workerTerminationReason CapatazEnv { capatazId, capatazName, notifyEvent } Worker { workerId, workerName, workerSpec, workerAsync }
  = do
    let WorkerSpec { workerTerminationPolicy } = workerSpec
    case workerTerminationPolicy of
      Infinity -> cancelWith
        workerAsync
        TerminateWorkerException {workerId , workerTerminationReason }

      BrutalTermination -> cancelWith
        workerAsync
        BrutallyTerminateWorkerException {workerId , workerTerminationReason }

      TimeoutMillis millis -> race_
        ( do
          threadDelay (millis * 1000)
          cancelWith
            workerAsync
            BrutallyTerminateWorkerException
              { workerId
              , workerTerminationReason
              }
        )
        ( cancelWith
          workerAsync
          TerminateWorkerException {workerId , workerTerminationReason }
        )

    eventTime <- getCurrentTime
    notifyEvent WorkerTerminated
      { capatazId
      , capatazName
      , eventTime
      , workerId
      , workerName
      , workerThreadId    = asyncThreadId workerAsync
      , terminationReason = workerTerminationReason
      }


-- | Internal sub-routine that terminates workers of a Capataz, used when a
-- Capataz instance is terminated
terminateWorkers :: Text -> CapatazEnv -> IO ()
terminateWorkers terminationReason env@CapatazEnv { capatazName, capatazId, capatazWorkerTerminationOrder, notifyEvent }
  = do
    eventTime <- getCurrentTime
    workerMap <- readWorkerMap env

    let workers =
          sortWorkersByTerminationOrder capatazWorkerTerminationOrder workerMap

    notifyEvent WorkersTerminationStarted
      { capatazName
      , capatazId
      , terminationReason
      , eventTime
      }

    forM_ workers (terminateWorker terminationReason env)

    notifyEvent WorkersTerminationFinished
      { capatazName
      , capatazId
      , terminationReason
      , eventTime
      }

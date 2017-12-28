{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Control.Concurrent.Internal.Supervisor.Child where

import Protolude

import Control.Concurrent.Async      (asyncWithUnmask)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Data.Time.Clock               (getCurrentTime)
import GHC.Conc                      (labelThread)

import qualified Data.Text    as T
import qualified Data.UUID.V4 as UUID

import Control.Concurrent.Internal.Supervisor.Types
import Control.Concurrent.Internal.Supervisor.Util
    (getTidNumber, readChildMap, sortChildrenByTerminationOrder)

setChildThreadName :: ChildId -> ChildName -> IO ()
setChildThreadName childId childName = do
  tid <- myThreadId
  let childIdentifier =
        T.unpack childName <> "_" <> show childId <> "_" <> maybe
          ""
          T.unpack
          (getTidNumber tid)
  labelThread tid childIdentifier

handleChildException
  :: (IO () -> IO a)
  -> SupervisorEnv
  -> ChildSpec
  -> ChildId
  -> RestartCount
  -> SomeException
  -> IO MonitorEvent
handleChildException unmask SupervisorEnv { supervisorId, supervisorName, notifyEvent } ChildSpec { childName, childOnFailure, childOnTermination } childId restartCount err
  = do
    childThreadId    <- myThreadId
    monitorEventTime <- getCurrentTime
    case fromException err of
      Just RestartChildException ->
        return ChildForcedRestart {childId , childName , monitorEventTime }

      Just TerminateChildException { childTerminationReason } -> do
        eErrResult <- try $ unmask childOnTermination

        notifyEvent SupervisedChildCallbackExecuted
          { supervisorId
          , supervisorName
          , childId
          , childName
          , childThreadId
          , childCallbackError = either Just (const Nothing) eErrResult
          , callbackType       = OnTermination
          , eventTime          = monitorEventTime
          }

        case eErrResult of
          Left childCallbackError -> return ChildFailed
            { childName
            , childId
            , monitorEventTime
            , childError        = toException ChildCallbackFailed
              { childId
              , childCallbackError
              , callbackType       = OnTermination
              , childActionError   = Just err
              }
            , childRestartCount = restartCount
            }
          Right _ -> return ChildTerminated
            { childId
            , childName
            , monitorEventTime
            , childTerminationReason
            , childRestartCount      = restartCount
            }

      Just BrutallyTerminateChildException { childTerminationReason } -> return
        ChildTerminated
          { childId
          , childName
          , monitorEventTime
          , childTerminationReason
          , childRestartCount      = restartCount
          }

      -- This exception was an error from the given sub-routine
      Nothing -> do
        eErrResult <- try $ unmask $ childOnFailure err

        notifyEvent SupervisedChildCallbackExecuted
          { supervisorId
          , supervisorName
          , childId
          , childName
          , childThreadId
          , childCallbackError = either Just (const Nothing) eErrResult
          , callbackType       = OnFailure
          , eventTime          = monitorEventTime
          }

        case eErrResult of
          Left childCallbackError -> return ChildFailed
            { childName
            , childId
            , monitorEventTime
            , childRestartCount = restartCount
            , childError        = toException ChildCallbackFailed
              { childId
              , childCallbackError
              , callbackType       = OnFailure
              , childActionError   = Just err
              }
            }
          Right _ -> return ChildFailed
            { childName
            , childId
            , monitorEventTime
            , childError        = err
            , childRestartCount = restartCount
            }

handleChildCompletion
  :: (IO () -> IO a)
  -> SupervisorEnv
  -> ChildSpec
  -> ChildId
  -> RestartCount
  -> IO MonitorEvent
handleChildCompletion unmask SupervisorEnv { supervisorId, supervisorName, notifyEvent } ChildSpec { childName, childOnCompletion } childId restartCount
  = do
    childThreadId    <- myThreadId
    monitorEventTime <- getCurrentTime
    eCompResult      <- try $ unmask childOnCompletion

    notifyEvent SupervisedChildCallbackExecuted
      { supervisorId
      , supervisorName
      , childId
      , childName
      , childThreadId
      , childCallbackError = either Just (const Nothing) eCompResult
      , callbackType       = OnCompletion
      , eventTime          = monitorEventTime
      }

    case eCompResult of
      Left err -> return ChildFailed
        { childName
        , childId
        , monitorEventTime
        , childError        = toException ChildCallbackFailed
          { childId
          , childCallbackError = err
          , callbackType       = OnCompletion
          , childActionError   = Nothing
          }
        , childRestartCount = restartCount
        }
      Right _ ->
        return ChildCompleted {childName , childId , monitorEventTime }

childMain :: SupervisorEnv -> ChildSpec -> ChildId -> RestartCount -> IO Child
childMain env@SupervisorEnv { supervisorQueue } childSpec@ChildSpec { childName, childAction } childId restartCount
  = do
    childCreationTime <- getCurrentTime
    childAsync        <- asyncWithUnmask $ \unmask -> do

      eResult <- try $ do
        setChildThreadName childId childName
        unmask childAction

      resultEvent <- case eResult of
        Left err ->
          handleChildException unmask env childSpec childId restartCount err
        Right _ ->
          handleChildCompletion unmask env childSpec childId restartCount

      atomically $ writeTQueue supervisorQueue (MonitorEvent resultEvent)

    return Child
      { childId
      , childName
      , childAsync
      , childCreationTime
      , childSpec
      }

notifyChildStarted :: Maybe (ChildId, Int) -> SupervisorEnv -> Child -> IO ()
notifyChildStarted mRestartInfo SupervisorEnv { supervisorId, supervisorName, notifyEvent } Child { childId, childName, childAsync }
  = do
    eventTime <- getCurrentTime
    case mRestartInfo of
      Just (_childId, childRestartCount) -> notifyEvent
        SupervisedChildRestarted
          { supervisorId
          , supervisorName
          , childId
          , childName
          , childRestartCount
          , childThreadId     = asyncThreadId childAsync
          , eventTime
          }
      Nothing -> notifyEvent SupervisedChildStarted
        { supervisorId
        , supervisorName
        , childId
        , childName
        , eventTime
        , childThreadId  = asyncThreadId childAsync
        }

forkChild
  :: SupervisorEnv -> ChildSpec -> Maybe (ChildId, RestartCount) -> IO Child
forkChild env childSpec mRestartInfo = do
  (childId, restartCount) <- case mRestartInfo of
    Just (childId, restartCount) -> pure (childId, restartCount)
    Nothing                      -> (,) <$> UUID.nextRandom <*> pure 0

  child <- childMain env childSpec childId restartCount
  notifyChildStarted mRestartInfo env child
  return child

terminateChild :: Text -> SupervisorEnv -> Child -> IO ()
terminateChild childTerminationReason SupervisorEnv { supervisorId, supervisorName, notifyEvent } Child { childId, childName, childSpec, childAsync }
  = do
    let ChildSpec { childTerminationPolicy } = childSpec
    case childTerminationPolicy of
      Infinity -> cancelWith
        childAsync
        TerminateChildException {childId , childTerminationReason }

      BrutalTermination -> cancelWith
        childAsync
        BrutallyTerminateChildException {childId , childTerminationReason }

      TimeoutMillis millis -> race_
        ( do
          threadDelay (millis * 1000)
          cancelWith
            childAsync
            BrutallyTerminateChildException {childId , childTerminationReason }
        )
        ( cancelWith
          childAsync
          TerminateChildException {childId , childTerminationReason }
        )

    eventTime <- getCurrentTime
    notifyEvent SupervisedChildTerminated
      { supervisorId
      , supervisorName
      , eventTime
      , childId
      , childName
      , childThreadId     = asyncThreadId childAsync
      , terminationReason = childTerminationReason
      }


terminateChildren :: Text -> SupervisorEnv -> IO ()
terminateChildren terminationReason env@SupervisorEnv { supervisorName, supervisorId, supervisorChildTerminationOrder, notifyEvent }
  = do
    eventTime <- getCurrentTime
    childMap  <- readChildMap env

    let children = sortChildrenByTerminationOrder
          supervisorChildTerminationOrder
          childMap

    notifyEvent SupervisedChildrenTerminationStarted
      { supervisorName
      , supervisorId
      , terminationReason
      , eventTime
      }

    forM_ children (terminateChild terminationReason env)

    notifyEvent SupervisedChildrenTerminationFinished
      { supervisorName
      , supervisorId
      , terminationReason
      , eventTime
      }

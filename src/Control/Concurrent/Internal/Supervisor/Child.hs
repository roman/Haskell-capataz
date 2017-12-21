{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Control.Concurrent.Internal.Supervisor.Child where

import Protolude

import Control.Concurrent.STM.TQueue (writeTQueue)
import Data.Time.Clock               (getCurrentTime)

import qualified Data.UUID.V4        as UUID

import Control.Concurrent.Internal.Supervisor.Types
import Control.Concurrent.Internal.Supervisor.Util  (
      appendChildToMap
    , resetChildMap
    , sortChildrenByTerminationOrder
    )

childMain :: SupervisorEnv -> ChildSpec -> ChildId -> RestartCount -> IO Child
childMain SupervisorEnv { supervisorQueue } childSpec@ChildSpec { childName, childAction, childOnFailure, childOnCompletion, childOnTermination } childId restartCount
  = do
    childCreationTime <- getCurrentTime
    childAsync        <- asyncWithUnmask $ \unmask -> do
      eResult          <- try $ unmask childAction
      monitorEventTime <- getCurrentTime
      resultEvent      <- case eResult of
        Left err -> case fromException err of
          Just TerminateChildException { childTerminationReason } -> do
            eErrResult <- try $ unmask childOnTermination
            case eErrResult of
              Left childCallbackError -> return ChildFailed
                { childName
                , childId
                , monitorEventTime
                , childError        = toException ChildCallbackFailed
                  { childId
                  , childCallbackError
                  , childActionError   = Just err
                  }
                , childRestartCount = restartCount
                }
              Right _ -> return ChildTerminated
                { childId
                , childName
                , monitorEventTime
                , childTerminationReason
                , childRestartCount = restartCount
                }

          Just (BrutallyTerminateChildException {childTerminationReason}) -> return ChildTerminated
                { childId
                , childName
                , monitorEventTime
                , childTerminationReason
                , childRestartCount = restartCount
                }


          Nothing -> do
            eErrResult <- try $ unmask $ childOnFailure err
            case eErrResult of
              Left childCallbackError -> return ChildFailed
                { childName
                , childId
                , monitorEventTime
                , childRestartCount = restartCount
                , childError        = toException ChildCallbackFailed
                  { childId
                  , childCallbackError
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
        Right _ -> do
          eCompResult <- try $ unmask childOnCompletion
          case eCompResult of
            Left err -> return ChildFailed
              { childName
              , childId
              , monitorEventTime
              , childError        = toException ChildCallbackFailed
                { childId
                , childCallbackError = err
                , childActionError   = Nothing
                }
              , childRestartCount = restartCount
              }
            Right _ ->
              return ChildCompleted {childName , childId , monitorEventTime }

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
  :: SupervisorEnv -> ChildSpec -> Maybe (ChildId, RestartCount) -> IO ChildId
forkChild env childSpec mRestartInfo = do
  (childId, restartCount) <- case mRestartInfo of
    Just (childId, restartCount) -> pure (childId, restartCount)
    Nothing                      -> (,) <$> UUID.nextRandom <*> pure 0

  child <- childMain env childSpec childId restartCount
  appendChildToMap   env          childId child
  notifyChildStarted mRestartInfo env     child
  return childId

terminateChild :: Text -> SupervisorEnv -> Child -> IO ()
terminateChild childTerminationReason SupervisorEnv { supervisorName, supervisorId, supervisorChildTerminationPolicy, notifyEvent } Child {childId, childName, childAsync}
  = do
    eventTime <- getCurrentTime
    notifyEvent
      SupervisedChildTerminated
        { supervisorName
        , supervisorId
        , childId
        , childName
        , eventTime
        , terminationReason = childTerminationReason
        , childThreadId     = asyncThreadId childAsync
        }

    case supervisorChildTerminationPolicy of
      BrutalTermination ->
        cancelWith childAsync
                   BrutallyTerminateChildException {childId , childTerminationReason }
      TimeoutSeconds n -> do
        cancelWith childAsync
                   TerminateChildException {childId , childTerminationReason }
        race_ (threadDelay (n * 1000000)) (wait childAsync)

terminateChildren :: Text -> SupervisorEnv -> IO ()
terminateChildren terminationReason env@SupervisorEnv { supervisorName, supervisorId, supervisorChildTerminationOrder, notifyEvent }
  = do
    eventTime <- getCurrentTime
    childMap <- resetChildMap env
    let children = sortChildrenByTerminationOrder supervisorChildTerminationOrder childMap

    notifyEvent
      SupervisedChildrenTerminationStarted
        { supervisorName
        , supervisorId
        , terminationReason
        , eventTime
        }

    forM_ children (terminateChild terminationReason env)

    notifyEvent
      SupervisedChildrenTerminationFinished
        { supervisorName
        , supervisorId
        , terminationReason
        , eventTime
        }

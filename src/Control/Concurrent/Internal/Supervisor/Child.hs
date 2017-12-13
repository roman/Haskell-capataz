{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Control.Concurrent.Internal.Supervisor.Child where

import Protolude

import Control.Concurrent.STM.TQueue (writeTQueue)
import Data.IORef                    (readIORef)
import Data.Time.Clock               (getCurrentTime)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.V4        as UUID

import Control.Concurrent.Internal.Supervisor.Types
import Control.Concurrent.Internal.Supervisor.Util  (appendChildToMap, removeChildFromMap)

childMain :: SupervisorEnv -> ChildSpec -> ChildId -> RestartCount -> IO Child
childMain SupervisorEnv { supervisorQueue } childSpec@ChildSpec { childName, childAction, childOnError, childOnCompletion, childOnTermination } childId restartCount
  = do
    childCreationTime <- getCurrentTime
    childAsync        <- asyncWithUnmask $ \unmask -> do
      eResult          <- try $ unmask childAction
      monitorEventTime <- getCurrentTime
      resultEvent      <- case eResult of
        Left err -> case fromException err of
          Just TerminateChildException{} -> do
            eErrResult <- try $ unmask $ childOnTermination
            case eErrResult of
              Left childCallbackError -> return ChildFailed
                { childName
                , childId
                , monitorEventTime
                , childError        = toException $ ChildCallbackException
                  { childId
                  , childCallbackError
                  , childActionError   = err
                  }
                , childRestartCount = succ restartCount
                }
              Right _ -> return ChildTerminated
                { childId
                , childName
                , monitorEventTime
                , childRestartCount = restartCount
                }

          Nothing -> do
            eErrResult <- try $ unmask $ childOnError err
            case eErrResult of
              Left childCallbackError -> return ChildFailed
                { childName
                , childId
                , monitorEventTime
                , childError        = toException $ ChildCallbackException
                  { childId
                  , childCallbackError
                  , childActionError   = err
                  }
                , childRestartCount = succ restartCount
                }
              Right _ -> return ChildFailed
                { childName
                , childId
                , monitorEventTime
                , childError        = err
                , childRestartCount = succ restartCount
                }
        Right _ -> do
          eCompResult <- try $ unmask childOnCompletion
          case eCompResult of
            Left err -> return ChildFailed
              { childName
              , childId
              , monitorEventTime
              , childError        = err
              , childRestartCount = succ restartCount
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
notifyChildStarted mRestartInfo SupervisorEnv {supervisorId, supervisorName, notifyEvent} Child {childId, childName, childAsync} = do
  eventTime <- getCurrentTime
  case mRestartInfo of
    Just (_childId, restartCount) ->
      notifyEvent SupervisedChildRestarted
        { supervisorId
        , supervisorName
        , childId
        , childName
        , eventTime
        , restartCount
        , childThreadId = asyncThreadId childAsync
        }
    Nothing ->
      notifyEvent SupervisedChildStarted
        { supervisorId
        , supervisorName
        , childId
        , childName
        , eventTime
        , childThreadId = asyncThreadId childAsync
        }

forkChild
  :: SupervisorEnv
  -> ChildSpec
  -> Maybe (ChildId, RestartCount)
  -> IO ChildId
forkChild env childSpec mRestartInfo = do
  (childId, restartCount) <-
    case mRestartInfo of
      Just (childId, restartCount) ->
        pure (childId, restartCount)
      Nothing ->
        ((,) <$> UUID.nextRandom <*> pure 0)

  child <- childMain env childSpec childId restartCount
  appendChildToMap env childId child
  notifyChildStarted mRestartInfo env child
  return childId

terminateChild :: Text -> SupervisorEnv -> ChildId -> IO ()
terminateChild terminationReason env@SupervisorEnv { supervisorName, supervisorId, notifyEvent } childId
  = removeChildFromMap env childId $ \Child { childName, childAsync } -> do
    eventTime <- getCurrentTime
    notifyEvent
      ( SupervisedChildTerminated
        { supervisorName
        , supervisorId
        , childId
        , childName
        , eventTime
        , terminationReason
        , childThreadId     = asyncThreadId childAsync
        }
      )
    cancelWith childAsync
               (TerminateChildException {childId , terminationReason })
    wait childAsync

terminateChildren :: Text -> SupervisorEnv -> IO ()
terminateChildren terminationReason env@SupervisorEnv { supervisorName, supervisorId, supervisorChildMap, notifyEvent }
  = do
    eventTime   <- getCurrentTime
    childrenIds <- HashMap.keys <$> readIORef supervisorChildMap

    notifyEvent
      ( SupervisedChildrenTerminationStarted
        { supervisorName
        , supervisorId
        , terminationReason
        , eventTime
        }
      )
    forM_ childrenIds $ \childId -> terminateChild terminationReason env childId

    notifyEvent
      ( SupervisedChildrenTerminationFinished
        { supervisorName
        , supervisorId
        , terminationReason
        , eventTime
        }
      )

restartChild :: SupervisorEnv -> ChildSpec -> ChildId -> RestartCount -> IO ()
restartChild supervisorEnv childSpec childId restartCount =
  void $ forkChild supervisorEnv childSpec (Just (childId, restartCount))

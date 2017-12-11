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
    childAsync        <- async $ do
      eResult          <- try childAction
      monitorEventTime <- getCurrentTime
      resultEvent      <- case eResult of
        Left err -> case fromException err of
          Just TerminateChildException{} -> do
            childOnTermination
            return ChildTerminated
              { childId
              , childName
              , monitorEventTime
              , childRestartCount = restartCount
              }

          Nothing -> do
            childOnError err
            return ChildFailed
              { childName
              , childId
              , monitorEventTime
              , childError        = err
              , childRestartCount = succ restartCount
              }
        Right _ -> do
          childOnCompletion
          return ChildCompleted {childName , childId , monitorEventTime }

      atomically $ writeTQueue supervisorQueue (MonitorEvent resultEvent)

    return Child
      { childId
      , childName
      , childAsync
      , childCreationTime
      , childSpec
      }

forkChild
  :: SupervisorEnv
  -> ChildSpec
  -> Maybe ChildId
  -> Maybe RestartCount
  -> IO ChildId
forkChild env childSpec mChildId mRestartCount = do
  let restartCount = fromMaybe 0 mRestartCount
  childId <- maybe UUID.nextRandom return mChildId
  child   <- childMain env childSpec childId restartCount
  appendChildToMap env childId child
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

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Control.Concurrent.Internal.Supervisor.Child where

import Protolude

import Control.Concurrent.STM.TQueue (writeTQueue)
import Data.Time.Clock               (getCurrentTime)

import qualified Data.UUID.V4 as UUID

import Control.Concurrent.Internal.Supervisor.Types
import Control.Concurrent.Internal.Supervisor.Util  (appendChildToMap)

childMain :: SupervisorEnv -> ChildSpec -> ChildId -> RestartCount -> IO Child
childMain (SupervisorEnv { supervisorQueue }) childSpec@(ChildSpec { childName, childAction }) childId restartCount
  = do

    childCreationTime <- getCurrentTime
    childAsync        <- async $ do
      eResult          <- try childAction
      monitorEventTime <- getCurrentTime
      result           <- case eResult of
        Left err -> case fromException err of
          Just (TerminateChildException{}) -> return $ ChildTerminated
            { childId
            , childName
            , monitorEventTime
            , childRestartCount = restartCount
            }

          Nothing -> return $ ChildFailed
            { childName
            , childId
            , monitorEventTime
            , childError        = err
            , childRestartCount = succ restartCount
            }
        Right _ ->
          return $ ChildCompleted {childName , childId , monitorEventTime }

      atomically $ writeTQueue supervisorQueue (MonitorEvent result)

    return $ Child {childId , childAsync , childCreationTime , childSpec }

forkChild
  :: SupervisorEnv
  -> ChildSpec
  -> Maybe ChildId
  -> Maybe RestartCount
  -> IO ChildId
forkChild env childSpec mChildId mRestartCount = do
  childId <- maybe UUID.nextRandom return mChildId
  child   <- childMain env childSpec childId restartCount
  appendChildToMap env childId child
  return childId
  where restartCount = fromMaybe 0 mRestartCount

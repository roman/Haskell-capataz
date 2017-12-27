{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Control.Concurrent.Internal.Supervisor.Restart where

import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

import Protolude

import qualified Control.Concurrent.Internal.Supervisor.Child as Child
import           Control.Concurrent.Internal.Supervisor.Types
import           Control.Concurrent.Internal.Supervisor.Util
    ( appendChildToMap
    , fetchChildEnv
    , readChildMap
    , removeChildFromMap
    , resetChildMap
    , sortChildrenByTerminationOrder
    )
import qualified Data.HashMap.Strict                          as HashMap

--------------------------------------------------------------------------------

calcDiffSeconds :: UTCTime -> IO NominalDiffTime
calcDiffSeconds creationTime = do
  currentTime <- getCurrentTime
  return $ diffUTCTime currentTime creationTime

calcRestartAction
  :: SupervisorEnv -> Int -> NominalDiffTime -> ChildRestartAction
calcRestartAction SupervisorEnv { supervisorIntensity, supervisorPeriodSeconds } restartCount diffSeconds
  = case () of
    _
      | diffSeconds
        <  supervisorPeriodSeconds
        && restartCount
        >  supervisorIntensity
      -> HaltSupervisor
      | diffSeconds > supervisorPeriodSeconds
      -> ResetRestartCount
      | otherwise
      -> IncreaseRestartCount

execSupervisorRestartStrategy :: SupervisorEnv -> ChildEnv -> Int -> IO ()
execSupervisorRestartStrategy supervisorEnv@SupervisorEnv { supervisorRestartStrategy } ChildEnv { childId, childSpec } childRestartCount
  = case supervisorRestartStrategy of
    AllForOne -> do
      newChildren <- restartChildren supervisorEnv childId childRestartCount
      let newChildrenMap =
            newChildren
              & fmap (\child@Child { childId = cid } -> (cid, child))
              & HashMap.fromList
      resetChildMap supervisorEnv (const newChildrenMap)

    OneForOne -> do
      removeChildFromMap supervisorEnv childId
      newChild <- restartChild supervisorEnv childSpec childId childRestartCount
      appendChildToMap supervisorEnv newChild

execRestartAction :: SupervisorEnv -> ChildEnv -> Int -> IO ()
execRestartAction supervisorEnv childEnv@ChildEnv { childId, childName, childCreationTime } childRestartCount
  = do
    restartAction <- calcRestartAction supervisorEnv childRestartCount
      <$> calcDiffSeconds childCreationTime

    case restartAction of
      HaltSupervisor -> throwIO SupervisorIntensityReached
        { childId
        , childName
        , childRestartCount
        }

      ResetRestartCount ->
        execSupervisorRestartStrategy supervisorEnv childEnv 0

      IncreaseRestartCount -> execSupervisorRestartStrategy
        supervisorEnv
        childEnv
        (succ childRestartCount)

--------------------------------------------------------------------------------

restartChildren :: SupervisorEnv -> ChildId -> RestartCount -> IO [Child]
restartChildren supervisorEnv@SupervisorEnv { supervisorChildTerminationOrder } failingChildId restartCount
  = do
    childMap <- readChildMap supervisorEnv

    let children = sortChildrenByTerminationOrder
          supervisorChildTerminationOrder
          childMap

    newChildren <- forM children $ \child@Child { childId, childSpec } -> do
      unless (failingChildId == childId) $ forceRestartChild supervisorEnv child

      let ChildSpec { childRestartStrategy } = childSpec
      case childRestartStrategy of
        Temporary -> return Nothing
        _         -> Just <$> restartChild supervisorEnv childSpec childId restartCount

    return $ catMaybes newChildren

forceRestartChild :: SupervisorEnv -> Child -> IO ()
forceRestartChild SupervisorEnv { supervisorName, supervisorId, notifyEvent } Child { childId, childName, childAsync }
  = do
    eventTime <- getCurrentTime
    notifyEvent SupervisedChildTerminated
      { supervisorName
      , supervisorId
      , childId
      , childName
      , eventTime
      , childThreadId     = asyncThreadId childAsync
      , terminationReason = "forced restart"
      }
    cancelWith childAsync RestartChildException

restartChild
  :: SupervisorEnv -> ChildSpec -> ChildId -> RestartCount -> IO Child
restartChild supervisorEnv childSpec childId restartCount =
  Child.forkChild supervisorEnv childSpec (Just (childId, restartCount))

--------------------------------------------------------------------------------

handleChildCompleted :: SupervisorEnv -> ChildId -> UTCTime -> IO ()
handleChildCompleted env@SupervisorEnv { supervisorName, supervisorId, notifyEvent } childId eventTime
  = do
    mChildEnv <- fetchChildEnv env childId
    case mChildEnv of
      Nothing -> return ()
      Just childEnv@ChildEnv { childName, childAsync, childRestartStrategy } ->
        do
          notifyEvent SupervisedChildCompleted
            { supervisorId
            , supervisorName
            , childId
            , childName
            , eventTime
            , childThreadId  = asyncThreadId childAsync
            }
          case childRestartStrategy of
            Permanent -> do
              -- NOTE: Completed children should never account as errors happening on
              -- a supervised thread, ergo, they should be restarted every time.

              -- TODO: Notify a warning around having a childRestartStrategy different
              -- than Temporary on children that may complete.
              let restartCount = 0
              execRestartAction env childEnv restartCount

            _ -> removeChildFromMap env childId

handleChildFailed :: SupervisorEnv -> ChildId -> SomeException -> Int -> IO ()
handleChildFailed env@SupervisorEnv { supervisorName, supervisorId, notifyEvent } childId childError restartCount
  = do
    mChildEnv <- fetchChildEnv env childId
    case mChildEnv of
      Nothing -> return ()
      Just childEnv@ChildEnv { childName, childAsync, childRestartStrategy } ->
        do
          eventTime <- getCurrentTime
          notifyEvent SupervisedChildFailed
            { supervisorName
            , supervisorId
            , childId
            , childName
            , childError
            , childThreadId  = asyncThreadId childAsync
            , eventTime
            }
          case childRestartStrategy of
            Temporary -> removeChildFromMap env childId
            _         -> execRestartAction env childEnv restartCount

handleChildTerminated :: SupervisorEnv -> ChildId -> Text -> Int -> IO ()
handleChildTerminated env@SupervisorEnv { supervisorName, supervisorId, notifyEvent } childId terminationReason childRestartCount
  = do
    mChildEnv <- fetchChildEnv env childId
    case mChildEnv of
      Nothing -> return ()
      Just childEnv@ChildEnv { childName, childAsync, childRestartStrategy } ->
        do
          eventTime <- getCurrentTime
          notifyEvent SupervisedChildTerminated
            { supervisorName
            , supervisorId
            , childId
            , childName
            , eventTime
            , terminationReason
            , childThreadId     = asyncThreadId childAsync
            }
          case childRestartStrategy of
            Permanent -> execRestartAction env childEnv childRestartCount
            _         -> removeChildFromMap env childId

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Concurrent.Internal.Supervisor.Restart where

import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)

import Protolude

import qualified Control.Concurrent.Internal.Supervisor.Child as Child
import Control.Concurrent.Internal.Supervisor.Util (resetChildMap, removeChildFromMap, sortChildrenByTerminationOrder)
import Control.Concurrent.Internal.Supervisor.Types


--------------------------------------------------------------------------------

calcDiffSeconds :: UTCTime -> IO NominalDiffTime
calcDiffSeconds creationTime = do
  currentTime <- getCurrentTime
  return $ diffUTCTime currentTime creationTime

calcRestartAction :: SupervisorEnv -> Int -> NominalDiffTime -> ChildRestartAction
calcRestartAction SupervisorEnv { supervisorIntensity, supervisorPeriodSeconds }
                  restartCount
                  diffSeconds =
  case () of
    _ | diffSeconds < supervisorPeriodSeconds &&
        restartCount > supervisorIntensity ->
        HaltSupervisor

      | diffSeconds > supervisorPeriodSeconds ->
        ResetRestartCount

      | otherwise ->
        IncreaseRestartCount

execSupervisorRestartStrategy :: SupervisorEnv -> ChildEnv -> Int -> IO ()
execSupervisorRestartStrategy
  supervisorEnv@(SupervisorEnv {supervisorRestartStrategy})
  ChildEnv { childId
           , childSpec }
  restartCount =

  case supervisorRestartStrategy of
    AllForOne childTerminationOrder ->
      restartChildren supervisorEnv childTerminationOrder restartCount

    OneForOne ->
      restartChild supervisorEnv childSpec childId restartCount

execRestartAction :: SupervisorEnv -> ChildEnv -> Int -> IO ()
execRestartAction supervisorEnv childEnv@(ChildEnv {childCreationTime}) restartCount = do
  restartAction <-
    calcRestartAction supervisorEnv restartCount
        <$> calcDiffSeconds childCreationTime

  case restartAction of
    HaltSupervisor ->
      panic "halt supervisor"

    ResetRestartCount ->
      execSupervisorRestartStrategy supervisorEnv childEnv 0

    IncreaseRestartCount ->
      execSupervisorRestartStrategy supervisorEnv childEnv (succ restartCount)

--------------------------------------------------------------------------------

restartChildren
  :: SupervisorEnv
  -> ChildTerminationOrder
  -> RestartCount
  -> IO ()
restartChildren supervisorEnv childTerminationOrder restartCount = do
  childMap <- resetChildMap supervisorEnv

  let
    children =
      sortChildrenByTerminationOrder childTerminationOrder childMap

  forM_ children $ \(Child {childId, childSpec}) ->
    restartChild supervisorEnv childSpec childId restartCount


restartChild :: SupervisorEnv -> ChildSpec -> ChildId -> RestartCount -> IO ()
restartChild supervisorEnv childSpec childId restartCount =
  void $ Child.forkChild supervisorEnv childSpec (Just (childId, restartCount))

--------------------------------------------------------------------------------

handleChildCompleted :: SupervisorEnv -> ChildId -> UTCTime -> IO ()
handleChildCompleted env@SupervisorEnv { supervisorName, supervisorId, notifyEvent } childId eventTime
  = do
    removeChildFromMap env childId
      $ \childEnv@ChildEnv { childName, childAsync, childRestartStrategy } -> do
          notifyEvent SupervisedChildCompleted
              { supervisorId
              , supervisorName
              , childId
              , childName
              , eventTime
              , childThreadId = asyncThreadId childAsync
              }
          case childRestartStrategy of
            Permanent -> do
              -- NOTE: Completed children should never account as errors happening on
              -- a supervised thread, ergo, they should be restarted every time.

              -- TODO: Notify a warning around having a childRestartStrategy different
              -- than Temporary on children that may complete.
              let restartCount = 0
              execRestartAction env childEnv restartCount

            _ -> return ()

handleChildFailed :: SupervisorEnv -> ChildId -> SomeException -> Int -> IO ()
handleChildFailed env@SupervisorEnv {supervisorName, supervisorId, notifyEvent} childId childError restartCount =
  removeChildFromMap env childId
    $ \childEnv@ChildEnv {childName, childRestartStrategy, childAsync} -> do
        eventTime <- getCurrentTime
        notifyEvent (SupervisedChildFailed { supervisorName
                                           , supervisorId
                                           , childId
                                           , childName
                                           , childError
                                           , childThreadId = asyncThreadId childAsync
                                           , eventTime
                                           })
        case childRestartStrategy of
          Temporary ->
            return ()
          _ ->
            execRestartAction env childEnv restartCount

handleChildTerminated :: SupervisorEnv -> ChildId -> Text -> Int -> IO ()
handleChildTerminated env@SupervisorEnv {supervisorName, supervisorId, notifyEvent} childId terminationReason childRestartCount =
  removeChildFromMap env childId
    $ \childEnv@ChildEnv {childName, childRestartStrategy, childAsync } -> do
        eventTime <- getCurrentTime
        notifyEvent SupervisedChildTerminated
                     { supervisorName
                     , supervisorId
                     , childId
                     , childName
                     , eventTime
                     , terminationReason
                     , childThreadId = asyncThreadId childAsync
                     }
        case childRestartStrategy of
          Temporary ->
            -- Child is dropped and never restarted
            return ()

          Transient ->
            -- Child is dropped and never restarted
            return ()

          Permanent ->
            execRestartAction env childEnv childRestartCount

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings        #-}
module Control.Concurrent.Supervisor.Internal.Restart where

import Protolude

import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

import Control.Concurrent.Supervisor.Internal.Child (forkChild)
import Control.Concurrent.Supervisor.Internal.Types
import Control.Concurrent.Supervisor.Internal.Util  (resetChildMap, sortChildrenByPolicy)

--------------------------------------------------------------------------------

emitChildDropped :: ChildEnv -> IO ()
emitChildDropped (ChildEnv { supervisorName
                           , supervisorId
                           , childName
                           , childId
                           , childAsync
                           , notifyEvent }) = do
  eventTime <- getCurrentTime
  notifyEvent
    (ChildDropped { supervisorId
                  , supervisorName
                  , childId
                  , childName
                  , eventTime
                  , childThreadId =
                      asyncThreadId childAsync
                  })

emitChildTerminated :: ChildEnv -> IO ()
emitChildTerminated (ChildEnv { supervisorName
                              , supervisorId
                              , childName
                              , childId
                              , childAsync
                              , notifyEvent }) = do
  eventTime <- getCurrentTime
  notifyEvent
    (ChildTerminated { supervisorName
                     , supervisorId
                     , childName
                     , childId
                     , eventTime
                     , childThreadId =
                         asyncThreadId childAsync
                     })

emitChildRestarted :: ChildEnv -> Text -> IO ()
emitChildRestarted (ChildEnv { supervisorName
                             , supervisorId
                             , childName
                             , childId
                             , notifyEvent })
                   eventRestartReason = do
  eventTime <- getCurrentTime
  notifyEvent
    (ChildRestarted { supervisorId
                    , supervisorName
                    , childId
                    , childName
                    , eventTime
                    , eventRestartReason
                    })

--------------------------------------------------------------------------------

calcDiffSeconds :: UTCTime -> IO NominalDiffTime
calcDiffSeconds creationTime = do
  currentTime <- getCurrentTime
  return $ diffUTCTime currentTime creationTime

calcRestartAction :: ChildEnv -> Int -> NominalDiffTime -> ChildRestartAction
calcRestartAction (ChildEnv { supervisorIntensity
                            , supervisorPeriodSeconds
                            })
                  restartCount
                  diffSeconds =
  case () of
    _ | diffSeconds <= supervisorPeriodSeconds &&
        restartCount >= supervisorIntensity ->
        HaltSupervisor

      | diffSeconds >= supervisorPeriodSeconds ->
        ResetRestartCount

      | otherwise ->
        IncreaseRestartCount

execSupervisorRestartStrategy :: ChildEnv -> Int -> IO ()
execSupervisorRestartStrategy (ChildEnv { supervisorEnv
                                        , childId
                                        , childSpec
                                        , supervisorRestartStrategy })
                              restartCount =
  case supervisorRestartStrategy of
    AllForOne terminationPolicy ->
      restartChildren supervisorEnv terminationPolicy restartCount

    OneForOne ->
      restartChild supervisorEnv childSpec childId restartCount

execRestartAction :: ChildEnv -> Int -> IO ()
execRestartAction childEnv@(ChildEnv {childCreationTime}) restartCount = do
  restartAction <-
    calcRestartAction childEnv restartCount
        <$> calcDiffSeconds childCreationTime

  case restartAction of
    HaltSupervisor ->
      panic "halt supervisor"

    ResetRestartCount ->
      execSupervisorRestartStrategy childEnv 0

    IncreaseRestartCount ->
      execSupervisorRestartStrategy childEnv (succ restartCount)

restartTerminatedChild :: ChildEnv -> IO ()
restartTerminatedChild childEnv@(ChildEnv {childRestartStrategy, childAsync}) =
  case childRestartStrategy of
    Temporary ->
      emitChildDropped childEnv

    Transient -> do
      wait childAsync
      emitChildTerminated childEnv

    _ -> do
      emitChildRestarted childEnv "terminated"
      execRestartAction childEnv 0

restartFinishedChild :: ChildEnv -> IO ()
restartFinishedChild childEnv@(ChildEnv {childRestartStrategy}) =
  case childRestartStrategy of
    Permanent -> do
      emitChildRestarted childEnv "finished"
      execRestartAction childEnv 0

    _ ->
      emitChildDropped childEnv

restartFailedChild :: ChildEnv -> Int -> IO ()
restartFailedChild childEnv@(ChildEnv {childRestartStrategy})
                   restartCount =
  case childRestartStrategy of
    Temporary -> do
      emitChildDropped childEnv
    _ -> do
      emitChildRestarted childEnv "failed"
      execRestartAction childEnv restartCount

ingestChildMonitorEvent
  :: ChildEnv
  -> MonitorEvent
  -> IO ()
ingestChildMonitorEvent childEnv event =
  case event of
    Terminated {} ->
      restartTerminatedChild childEnv

    Failed {} ->
      restartFailedChild childEnv (childRestartCount event)

    Finished {} ->
      restartFinishedChild childEnv

--------------------------------------------------------------------------------

restartChildren
  :: SupervisorEnv
  -> SupervisorTerminationPolicy
  -> RestartCount
  -> IO ()
restartChildren supervisorEnv terminationPolicy restartCount = do
  childMap <- resetChildMap supervisorEnv

  let
    children =
      sortChildrenByPolicy terminationPolicy childMap

  forM_ children $ \(Child {childId, childSpec}) ->
    restartChild supervisorEnv childSpec childId restartCount

--------------------------------------------------------------------------------

restartChild
  :: SupervisorEnv
  -> ChildSpec
  -> ChildId
  -> RestartCount
  -> IO ()
restartChild supervisorEnv childSpec childId restartCount =
  void $ forkChild supervisorEnv childSpec (Just childId) (Just restartCount)

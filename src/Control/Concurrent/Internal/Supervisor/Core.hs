{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
module Control.Concurrent.Internal.Supervisor.Core where

import Protolude

import Control.Concurrent.MVar       (newEmptyMVar, takeMVar)
import Control.Concurrent.STM        (atomically)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar   (newTVarIO)
import Control.Teardown              (newTeardown)
import Data.IORef                    (newIORef)
import Data.Time.Clock               (UTCTime)
import Data.Time.Clock               (getCurrentTime)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.V4        as UUID (nextRandom)

import qualified Control.Concurrent.Internal.Supervisor.Child as Child

import Control.Concurrent.Internal.Supervisor.Types
import Control.Concurrent.Internal.Supervisor.Util
    ( childOptionsToSpec
    , readSupervisorStatus
    , removeChildFromMap
    , runtimeToEnv
    , sendSyncControlMsg
    , writeSupervisorStatus
    )

--------------------------------------------------------------------------------

handleChildCompleted :: SupervisorEnv -> ChildId -> UTCTime -> IO ()
handleChildCompleted env@SupervisorEnv { supervisorName, supervisorId, notifyEvent } childId eventTime
  = do
    removeChildFromMap env childId
      $ \Child { childName, childAsync, childSpec } -> do
          let ChildSpec { childRestartStrategy } = childSpec
          case childRestartStrategy of
            Permanent -> do
              -- NOTE: Completed children should never account as errors happening on
              -- a supervised thread, ergo, they should be restarted every time.

              -- TODO: Notify a warning around having a childRestartStrategy different
              -- than Temporal on children that may complete.

              let restartCount = 0
              Child.restartChild env childSpec childId restartCount

            _ -> notifyEvent SupervisedChildCompleted
              { supervisorId
              , supervisorName
              , childId
              , childName
              , eventTime
              , childRestartStrategy
              , childThreadId        = asyncThreadId childAsync
              }

handleMonitorEvent :: SupervisorEnv -> MonitorEvent -> IO Bool
handleMonitorEvent env monitorEv = do
  case monitorEv of
    ChildCompleted { childId, monitorEventTime } ->
      handleChildCompleted env childId monitorEventTime

    ChildTerminated{} -> panic "pending"

    ChildFailed{}     -> panic "pending"

  return True

handleControlAction :: SupervisorEnv -> ControlAction -> IO Bool
handleControlAction env controlAction = case controlAction of
  ForkChild { childSpec, returnChildId } -> do
    childId <- Child.forkChild env childSpec Nothing Nothing
    returnChildId childId
    return True

  TerminateChild { terminationReason, childId, notifyChildTermination } -> do
    Child.terminateChild terminationReason env childId
    notifyChildTermination
    return True

  TerminateSupervisor { notifySupervisorTermination } -> do
    Child.terminateChildren "supervisor shutdown" env
    notifySupervisorTermination
    writeSupervisorStatus env Halted
    return False

handleSupervisorMessage :: SupervisorEnv -> SupervisorMessage -> IO Bool
handleSupervisorMessage env message = case message of
  ControlAction controlAction -> handleControlAction env controlAction
  MonitorEvent  monitorEvent  -> handleMonitorEvent env monitorEvent

runSupervisorLoop :: (forall b . IO b -> IO b) -> SupervisorEnv -> IO ()
runSupervisorLoop unmask env@SupervisorEnv { supervisorId, supervisorName, supervisorStatusVar, supervisorQueue, notifyEvent }
  = do
    (status, message) <-
      atomically
      $   (,)
      <$> readSupervisorStatus supervisorStatusVar
      <*> readTQueue supervisorQueue

    case status of
      Initializing -> do
        eventTime <- getCurrentTime
        notifyEvent InvalidSupervisorStatusReached
          { supervisorId
          , supervisorName
          , eventTime
          }
        runSupervisorLoop unmask env

      Running -> do
        eContinueLoop <- unmask $ try $ handleSupervisorMessage env message
        case eContinueLoop of
          Left supervisorError -> do
            eventTime <- getCurrentTime
            notifyEvent SupervisorFailed
              { supervisorId
              , supervisorName
              , supervisorError
              , eventTime
              }
          Right continueLoop
            | continueLoop -> runSupervisorLoop unmask env
            | otherwise -> do
              eventTime <- getCurrentTime
              notifyEvent SupervisorTerminated
                { supervisorId
                , supervisorName
                , eventTime
                }

      Halted -> panic "pending implementation"

buildSupervisorRuntime :: SupervisorSpec -> IO SupervisorRuntime
buildSupervisorRuntime supervisorSpec = do
  supervisorId        <- UUID.nextRandom
  supervisorQueue     <- newTQueueIO
  supervisorStatusVar <- newTVarIO Initializing
  supervisorChildMap  <- newIORef HashMap.empty
  return SupervisorRuntime {..}

forkSupervisor :: SupervisorSpec -> IO Supervisor
forkSupervisor supervisorSpec@SupervisorSpec { supervisorName } = do
  supervisorRuntime <- buildSupervisorRuntime supervisorSpec

  let supervisorEnv = runtimeToEnv supervisorRuntime

  supervisorAsync <- asyncWithUnmask
    $ \unmask -> runSupervisorLoop unmask supervisorEnv

  writeSupervisorStatus supervisorEnv Running

  supervisorTeardown <- newTeardown
    ("supervisor[" <> supervisorName <> "]")
    (sendSyncControlMsg supervisorEnv TerminateSupervisor)

  return Supervisor {..}

forkChild :: ChildOptions -> IO () -> Supervisor -> IO ChildId
forkChild childOptions childAction Supervisor { supervisorEnv } = do
  let childSpec = childOptionsToSpec childOptions childAction
      SupervisorEnv { supervisorQueue } = supervisorEnv

  childIdVar <- newEmptyMVar
  atomically $ writeTQueue
    supervisorQueue
    ( ControlAction ForkChild
      { childSpec
      , childRestartCount = 0
      , returnChildId     = putMVar childIdVar
      }
    )
  childId <- takeMVar childIdVar
  return childId

terminateChild :: Text -> ChildId -> Supervisor -> IO ()
terminateChild terminationReason childId Supervisor { supervisorEnv } =
  sendSyncControlMsg
    supervisorEnv
    ( \notifyChildTermination ->
      TerminateChild {terminationReason , childId , notifyChildTermination }
    )

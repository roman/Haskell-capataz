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
import Data.Time.Clock               (getCurrentTime)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.V4        as UUID (nextRandom)

import qualified Control.Concurrent.Internal.Supervisor.Child   as Child
import qualified Control.Concurrent.Internal.Supervisor.Restart as Restart

import Control.Concurrent.Internal.Supervisor.Types
import Control.Concurrent.Internal.Supervisor.Util
    ( childOptionsToSpec
    , readSupervisorStatus
    , readSupervisorStatusSTM
    , sendSyncControlMsg
    , supervisorToEnv
    , withChild
    , writeSupervisorStatus
    )

--------------------------------------------------------------------------------

handleMonitorEvent :: SupervisorEnv -> MonitorEvent -> IO Bool
handleMonitorEvent env monitorEv = do
  case monitorEv of
    ChildCompleted { childId, monitorEventTime } ->
      Restart.handleChildCompleted env childId monitorEventTime

    ChildFailed { childId, childError, childRestartCount } ->
      Restart.handleChildFailed env childId childError childRestartCount

    ChildTerminated { childId, childRestartCount, childTerminationReason } ->
      Restart.handleChildTerminated env
                                    childId
                                    childTerminationReason
                                    childRestartCount


  return True

handleControlAction :: SupervisorEnv -> ControlAction -> IO Bool
handleControlAction env controlAction = case controlAction of
  ForkChild { childSpec, returnChildId } -> do
    childId <- Child.forkChild env childSpec Nothing
    returnChildId childId
    return True

  TerminateChild { terminationReason, childId, notifyChildTermination } -> do
    withChild env childId $ \child -> do
      Child.terminateChild terminationReason env child
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
    loopResult <-
      unmask
      $   try
      $   atomically
      $   (,)
      <$> readSupervisorStatusSTM supervisorStatusVar
      <*> readTQueue supervisorQueue

    case loopResult of
      Left supervisorError -> do
        eventTime <- getCurrentTime
        notifyEvent SupervisorFailed
          { supervisorId
          , supervisorName
          , supervisorError
          , eventTime
          }
        Child.terminateChildren "supervisor shutdown" env
        writeSupervisorStatus   env                   Halted
        throwIO supervisorError

      Right (status, message) -> case status of
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
              Child.terminateChildren "supervisor shutdown" env
              writeSupervisorStatus   env                   Halted
              throwIO supervisorError

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

buildSupervisorRuntime :: SupervisorOptions -> IO SupervisorRuntime
buildSupervisorRuntime supervisorOptions = do
  supervisorId        <- UUID.nextRandom
  supervisorQueue     <- newTQueueIO
  supervisorStatusVar <- newTVarIO Initializing
  supervisorChildMap  <- newIORef HashMap.empty
  return SupervisorRuntime {..}

forkSupervisor :: SupervisorOptions -> IO Supervisor
forkSupervisor supervisorOptions@SupervisorOptions { supervisorName, supervisorChildSpecList }
  = do
    supervisorRuntime <- buildSupervisorRuntime supervisorOptions

    let supervisorEnv = supervisorToEnv supervisorRuntime

    supervisorAsync <- asyncWithUnmask
      $ \unmask -> runSupervisorLoop unmask supervisorEnv

    forM_ supervisorChildSpecList
          (\childSpec -> Child.forkChild supervisorEnv childSpec Nothing)

    writeSupervisorStatus supervisorEnv Running

    supervisorTeardown <- newTeardown
      ("supervisor[" <> supervisorName <> "]")
      ( do
        status <- readSupervisorStatus supervisorEnv
        case status of
          Halted -> return ()
          _      -> sendSyncControlMsg supervisorEnv TerminateSupervisor
      )

    return Supervisor {..}

forkChild :: ChildOptions -> IO () -> Supervisor -> IO ChildId
forkChild childOptions childAction Supervisor { supervisorEnv } = do
  let childSpec = childOptionsToSpec childOptions childAction
      SupervisorEnv { supervisorQueue } = supervisorEnv

  childIdVar <- newEmptyMVar
  atomically $ writeTQueue
    supervisorQueue
    (ControlAction ForkChild {childSpec , returnChildId = putMVar childIdVar})
  takeMVar childIdVar

terminateChild :: Text -> ChildId -> Supervisor -> IO ()
terminateChild terminationReason childId Supervisor { supervisorEnv } =
  sendSyncControlMsg
    supervisorEnv
    ( \notifyChildTermination ->
      TerminateChild {terminationReason , childId , notifyChildTermination }
    )

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
module Control.Concurrent.Internal.Supervisor.Core where

import Protolude

import Control.Concurrent.Async      (asyncWithUnmask)
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
    ( appendChildToMap
    , childOptionsToSpec
    , fetchChild
    , readSupervisorStatus
    , readSupervisorStatusSTM
    , resetChildMap
    , sendSyncControlMsg
    , supervisorToEnv
    , writeSupervisorStatus
    )

--------------------------------------------------------------------------------

haltSupervisor :: SupervisorEnv -> IO ()
haltSupervisor env = do
  writeSupervisorStatus env Halting
  Child.terminateChildren "supervisor shutdown" env
  resetChildMap           env                   (const HashMap.empty)
  writeSupervisorStatus   env                   Halted

handleMonitorEvent :: SupervisorEnv -> MonitorEvent -> IO Bool
handleMonitorEvent env monitorEv = do
  case monitorEv of
    ChildForcedRestart{} ->
      -- We do nothing, as restart is being handled on restartChildren
      -- sub-routine
      return ()

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
    child@Child { childId } <- Child.forkChild env childSpec Nothing
    appendChildToMap env child
    returnChildId childId
    return True

  TerminateChild { terminationReason, childId, notifyChildTermination } -> do
    mChild <- fetchChild env childId
    case mChild of
      Nothing    -> return True
      Just child -> do
        Child.terminateChild terminationReason env child
        -- removeChildFromMap env childId
        notifyChildTermination
        return True

  TerminateSupervisor { notifySupervisorTermination } -> do
    haltSupervisor env
    notifySupervisorTermination
    return False

handleSupervisorMessage :: SupervisorEnv -> SupervisorMessage -> IO Bool
handleSupervisorMessage env message = case message of
  ControlAction controlAction -> handleControlAction env controlAction
  MonitorEvent  monitorEvent  -> handleMonitorEvent env monitorEvent

handleSupervisorException :: SupervisorEnv -> SomeException -> IO ()
handleSupervisorException env@SupervisorEnv { supervisorId, supervisorName, notifyEvent } supervisorError
  = do
    eventTime <- getCurrentTime
    notifyEvent SupervisorFailed
      { supervisorId
      , supervisorName
      , supervisorError
      , eventTime
      }
    haltSupervisor env
    throwIO supervisorError

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
      Left  supervisorError   -> handleSupervisorException env supervisorError

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
          eContinueLoop <- try $ unmask $ handleSupervisorMessage env message
          case eContinueLoop of
            Left supervisorError ->
              handleSupervisorException env supervisorError

            Right continueLoop
              | continueLoop -> runSupervisorLoop unmask env
              | otherwise -> do
                eventTime <- getCurrentTime
                notifyEvent SupervisorTerminated
                  { supervisorId
                  , supervisorName
                  , eventTime
                  }

        Halting ->
          -- Discard messages when halting
          return ()

        Halted ->
          panic "TODO: Pending halted state"

buildSupervisorRuntime :: SupervisorOptions -> IO SupervisorRuntime
buildSupervisorRuntime supervisorOptions = do
  supervisorId        <- UUID.nextRandom
  supervisorQueue     <- newTQueueIO
  supervisorStatusVar <- newTVarIO Initializing
  supervisorChildMap  <- newIORef HashMap.empty
  return SupervisorRuntime {..}

forkSupervisor :: SupervisorOptions -> IO Supervisor
forkSupervisor supervisorOptions@SupervisorOptions { supervisorName, supervisorChildSpecList, notifyEvent }
  = do
    supervisorRuntime <- buildSupervisorRuntime supervisorOptions

    let supervisorEnv@SupervisorEnv {supervisorId} = supervisorToEnv supervisorRuntime

    supervisorAsync <- asyncWithUnmask
      $ \unmask -> runSupervisorLoop unmask supervisorEnv

    forM_
      supervisorChildSpecList
      ( \childSpec -> do
        child <- Child.forkChild supervisorEnv childSpec Nothing
        appendChildToMap supervisorEnv child
      )

    writeSupervisorStatus supervisorEnv Running

    supervisorTeardown <- newTeardown
      ("supervisor[" <> supervisorName <> "]")
      ( do
        status <- readSupervisorStatus supervisorEnv
        case status of
          Halted -> return ()
          Halting -> return ()
          _      -> do
            eventTime <- getCurrentTime
            notifyEvent SupervisorShutdownInvoked {
                supervisorId
              , supervisorName
              , eventTime
              }
            sendSyncControlMsg supervisorEnv TerminateSupervisor
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

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE NoImplicitPrelude        #-}
module Control.Concurrent.Supervisor.Internal.Child where

import Protolude

import Control.Concurrent.STM        (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Data.IORef                    (atomicModifyIORef')
import Data.Time.Clock               (getCurrentTime)

import qualified Data.HashMap.Strict as H
import qualified Data.UUID.V4        as UUID (nextRandom)

import Control.Concurrent.Supervisor.Internal.Types
import Control.Concurrent.Supervisor.Internal.Util  (removeChild, resetChildMap)

--------------------------------------------------------------------------------

childMain
  :: SupervisorEnv
  -> ChildSpec
  -> ChildId
  -> RestartCount
  -> IO Child
childMain (SupervisorEnv { supervisorEventQueue
                         , supervisorChildShutdownTimeoutSeconds
                         })
          childSpec@(ChildSpec { childName
                               , childAction
                               , childOnError
                               , childOnFinished
                               })
          childId
          restartCount = do

  childCreationTime <- getCurrentTime
  childAsync <- async $ do
    eResult <- try childAction
    eventTime <- getCurrentTime
    result <-
      case eResult of
        Left err ->
          case fromException err of
            Just (TerminateChild {}) ->
              return
                $ Terminated {
                  childId
                , childName
                , eventTime
                , childRestartCount = restartCount
                }

            Nothing -> do
              race_ (threadDelay $ supervisorChildShutdownTimeoutSeconds * 1000100)
                    (childOnError err)
              return
                $ Failed { childName
                         , childId
                         , eventTime
                         , eventError =
                             err
                         , childRestartCount =
                             succ restartCount
                         }
        Right _ -> do
          race_ (threadDelay $ supervisorChildShutdownTimeoutSeconds * 1000100)
                childOnFinished
          return
            $ Finished { childName
                       , childId
                       , eventTime
                       }

    atomically
      $ writeTQueue supervisorEventQueue result

  return $ Child {childId, childAsync, childCreationTime, childSpec}

forkChild
  :: SupervisorEnv
  -> ChildSpec
  -> Maybe ChildId
  -> Maybe RestartCount
  -> IO ChildId
forkChild supervisorEnv@(SupervisorEnv {supervisorChildMap}) childSpec mChildId mRestartCount = do
    childId <- maybe UUID.nextRandom return mChildId
    child   <- childMain supervisorEnv childSpec childId restartCount
    atomicModifyIORef' supervisorChildMap
                       (\childMap -> (appendChild childId child childMap, ()))
    return childId
  where
    restartCount =
      fromMaybe 0 mRestartCount

    appendChild childId child =
      H.alter (const $ Just child) childId

--------------------------------------------------------------------------------

terminateChild :: SupervisorEnv -> Text -> ChildId -> IO ()
terminateChild supervisorEnv@(SupervisorEnv { notifyEvent })
               terminationReason
               childId = do

  removeChild supervisorEnv childId
    $ \(ChildEnv { supervisorId
                 , supervisorName
                 , childName
                 , childAsync
                 }) -> do

      eventTime <- getCurrentTime
      notifyEvent (ChildTerminated { supervisorName
                                   , supervisorId
                                   , childId
                                   , childName
                                   , eventTime
                                   , childThreadId =
                                       asyncThreadId childAsync
                                   })
      cancelWith childAsync (TerminateChild {childId, terminationReason})

terminateChildren :: SupervisorEnv -> Text -> IO ()
terminateChildren supervisorEnv terminationReason = do
  childMap <- resetChildMap supervisorEnv

  let
    children =
      H.elems childMap

  forM_ children $ \(Child {childId}) ->
    terminateChild supervisorEnv terminationReason childId

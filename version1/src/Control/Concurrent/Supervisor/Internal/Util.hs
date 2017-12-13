{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE RecordWildCards          #-}
module Control.Concurrent.Supervisor.Internal.Util where

import Protolude

import Data.IORef                    (IORef, atomicModifyIORef')
import Data.Time.Clock               (getCurrentTime)

import qualified Data.HashMap.Strict as H

import Control.Concurrent.Supervisor.Internal.Types

--------------------------------------------------------------------------------

resetChildMap :: SupervisorEnv -> IO ChildMap
resetChildMap (SupervisorEnv {supervisorChildMap}) =
  atomicModifyIORef' supervisorChildMap (\childMap -> (H.empty, childMap))

sortChildrenByPolicy
  :: SupervisorTerminationPolicy
  -> ChildMap
  -> [Child]
sortChildrenByPolicy terminationPolicy childMap =
    case terminationPolicy of
      OldestFirst ->
        children
      NewestFirst ->
        reverse children
  where
    -- NOTE: dissambiguates childCreationTime field
    childCreationTime' (Child {childCreationTime}) =
      childCreationTime

    children =
      sortBy (comparing childCreationTime')
             (H.elems childMap)


takeChild :: ChildId -> IORef ChildMap -> IO (Maybe Child)
takeChild childId supervisorChildMap = do
  atomicModifyIORef'
    supervisorChildMap
    (\childMap -> ( H.delete childId childMap
                  , H.lookup childId childMap
                  ))

removeChild :: SupervisorEnv -> ChildId -> (ChildEnv -> IO ()) -> IO ()
removeChild supervisorEnv@(SupervisorEnv { supervisorName
                                       , supervisorId
                                       , notifyEvent
                                       , supervisorChildMap
                                       })
          childId
          actionFn = do

  mChild <- takeChild childId supervisorChildMap

  case mChild of
    Nothing -> do
      eventTime <- getCurrentTime
      notifyEvent (ChildAlreadyHalted {supervisorName,
                                       supervisorId,
                                       childId,
                                       eventTime})
    Just child ->
      actionFn (toChildEnv supervisorEnv child)

--------------------------------------------------------------------------------

toChildEnv :: SupervisorEnv -> Child -> ChildEnv
toChildEnv supervisorEnv@(SupervisorEnv {..})
           (Child {..}) =
  -- MAGIC!
  let
    (ChildSpec {..}) =
      childSpec
  in
    ChildEnv {..}


toSupervisorEnv :: SupervisorRuntime -> SupervisorEnv
toSupervisorEnv (SupervisorRuntime {..}) =
  -- MAGIC!
  let
    (SupervisorOptions {..}) =
      supervisorOptions
  in
    SupervisorEnv {..}

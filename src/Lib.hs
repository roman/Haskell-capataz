{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Example of a library file. It is also used for testing the test suites.
module Lib where

import Protolude hiding (try)

import Data.Time.Clock (UTCTime, getCurrentTime)

import Control.Exception.Safe (try)
import Data.Default           (Default (..))
import Data.HashMap.Strict    (HashMap)
import Data.IORef             (IORef, atomicModifyIORef, newIORef)

import           Data.UUID    (UUID)
import qualified Data.UUID.V4 as UUID (nextRandom)

import Control.Concurrent.STM        (atomically, retry)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar   (TVar, newTVarIO, readTVar, writeTVar)

import qualified Data.HashMap.Strict as H

--------------------------------------------------------------------------------

type SupervisorId = UUID
type ChildId = UUID
type SupervisorName = Text
type ChildName = Text
type StartTime = UTCTime
type StartupDelay = Int
type TerminationTimespan = Int
type ChildThreadId = ThreadId

data SupervisorEvent
  = SupervisorStateError
    { seSupervisorName :: !SupervisorName
    , seSupervisorId   :: !SupervisorId
    , seErrorMessage   :: !Text }
  | SupervisorStarted
    {
      seSupervisorName :: !SupervisorName
    , seSupervisorId   :: !SupervisorId
    , seEventTime      :: !UTCTime
    }
  | SupervisorHalted
    {
      seSupervisorName :: !SupervisorName
    , seSupervisorId   :: !SupervisorId
    , seEventTime      :: !UTCTime
    }
  | ChildAlreadyHalted
    { seSupervisorName :: !SupervisorName
    , seSupervisorId   :: !SupervisorId
    , seChildId        :: !ChildId
    , seEventTime      :: !UTCTime }
  | ChildStarted
    { seSupervisorName :: !SupervisorName
    , seSupervisorId   :: !SupervisorId
    , seChildThreadId  :: !ChildThreadId
    , seChildId        :: !ChildId
    , seChildName      :: !ChildName
    , seEventTime      :: !UTCTime }
  | ChildRestarted
    { seSupervisorName :: !SupervisorName
    , seSupervisorId   :: !SupervisorId
    , seChildId        :: !ChildId
    , seChildName      :: !ChildName
    , seEventTime      :: !UTCTime
    , seChildEvent     :: !ChildEvent }
  | ChildTerminated
    { seSupervisorName :: !SupervisorName
    , seSupervisorId   :: !SupervisorId
    , seChildThreadId  :: !ChildThreadId
    , seChildId        :: !ChildId
    , seChildName      :: !ChildName
    , seEventTime      :: !UTCTime }
  | ChildDropped
    { seSupervisorName :: !SupervisorName
    , seSupervisorId   :: !SupervisorId
    , seChildThreadId  :: !ChildThreadId
    , seChildId        :: !ChildId
    , seChildName      :: !ChildName
    , seEventTime      :: !UTCTime }

data SupervisorTerminationPolicy
  = NewestFirst -- ^ Terminate child threads from most recent to oldest
  | OldestFirst -- ^ Terminate child threads from oldest to most recent
  deriving (Generic, Show, Eq, Ord)

instance Default SupervisorTerminationPolicy where
  def = OldestFirst

instance NFData SupervisorTerminationPolicy

data SupervisorRestartStrategy
  = AllForOne !SupervisorTerminationPolicy -- ^ Terminate all children threads when one fails and restart them all
  | OneForOne -- ^ Only restart child thread that terminated
  deriving (Generic, Show, Eq, Ord)

instance Default SupervisorRestartStrategy where
  def = OneForOne

instance NFData SupervisorRestartStrategy

data SupervisorFlags
  = SupervisorFlags {
      supRestartStrategy :: !SupervisorRestartStrategy
    , supRestartDelay    :: !(SomeException -> RestartCount -> StartTime -> Maybe StartupDelay) -- ^ Function that takes input variables into consideration calculate a delay on restart
    }
  deriving (Generic)

instance NFData SupervisorFlags

data ChildRestartStrategy
  = Permanent -- ^ Child thread is always restarted on completion
  | Transient -- ^ Child thread is restarted only if completed with failure
  | Temporal  -- ^ Child thread is never restarted on completion
  deriving (Generic, Show, Eq)

instance NFData ChildRestartStrategy

type RestartCount = Int


data ChildEvent
  -- ^ Indicates when a child thread finished without any errors
  = ChildCompleted { ceChildName    :: !ChildName
                   , ceChildId      :: !ChildId
                   , ceRestartCount :: !RestartCount
                   , ceStartTime    :: !StartTime }
  -- ^ Indicates when a child thread finished with errors
  | ChildFailed { ceChildName      :: !ChildName
                , ceChildId        :: !ChildId
                , ceRestartCount   :: !RestartCount
                , ceStartTime      :: !StartTime
                , ceChildException :: !SomeException }
  -- ^ Indicates when a child thread was finished from supervisor
  | ChildKilled { ceChildName    :: !ChildName
                , ceChildId      :: !ChildId
                , ceRestartCount :: !RestartCount
                , ceStartTime    :: !StartTime }
  deriving (Generic, Show)

data ChildSpec
  = ChildSpec {
      childSpecName        :: !ChildName  -- ^ Child name (tracing purposes)
    , childRestartStrategy :: !ChildRestartStrategy -- ^ Restart strategy for child thread
    , childSpecOnFailure   :: !(SomeException -> IO ()) -- ^ Callback executed on child failure
    , childSpecOnCompleted :: !(IO ()) -- ^ Callback executed on child successful exit
    , childSpecAction      :: !(IO ()) -- ^ Child action to be monitored
    }
    deriving (Generic)

data ChildRuntime
  = ChildRuntime {
      crRestartCountRef   :: !(IORef RestartCount)
    , crChildId           :: !ChildId
    , crChildCreationTime :: !UTCTime
    , crChildSpec         :: !ChildSpec
    }
    deriving (Generic)

data ChildProcess
  = ChildProcess {
      childAsync           :: !(Async ())
    , childRestartCountRef :: !(IORef RestartCount)
    , childId              :: !ChildId
    , childCreationTime    :: !UTCTime
    , childSpec            :: !ChildSpec
    }
    deriving (Generic)

data SupervisorSpec
  = SupervisorSpec {
      supName          :: !SupervisorName
    , supRestartPolicy :: !SupervisorRestartStrategy
    , supFlags         :: !SupervisorFlags
    , supChildSpecList :: ![ChildSpec]
    , supNotifyEvent   :: !(SupervisorEvent -> IO ())
    }
  deriving (Generic)

data SupervisorStatus
  = SupInit
  | SupRunning
  | SupHalting
  | SupHalted
  deriving (Generic, Show, Eq)

instance NFData SupervisorStatus

data SupervisorRuntime
  = SupervisorRuntime {
      srSupId       :: !SupervisorId
    , srSupEvQueue  :: !(TQueue ChildEvent)
    , srSupChildMap :: !(IORef (HashMap ChildId ChildProcess))
    , srSupStatus   :: !(TVar SupervisorStatus)
    , srSupSpec     :: !SupervisorSpec
    }
  deriving (Generic)

data Supervisor
  = Supervisor {
      supAsync    :: !(Async ())
    , supRuntime  :: !SupervisorRuntime
    }
  deriving (Generic)

data ChildException
  = KillChild {
      exChildId    :: ChildId
    , exKillReason :: Text
    }
    deriving (Generic, Show)

instance Exception ChildException
instance NFData ChildException

--------------------------------------------------------------------------------

_forkChild
  :: SupervisorRuntime
  -> Maybe ChildId
  -> Maybe RestartCount
  -> ChildSpec
  -> IO ()
_forkChild (SupervisorRuntime {..}) mChildId mRestartCount spec = do
        cId <- maybe UUID.nextRandom return mChildId
        restartCountRef <- newIORef (fromMaybe 0 mRestartCount)
        childRuntime <- buildChildRuntime cId restartCountRef
        atomicModifyIORef srSupChildMap
                          (\hsh -> (H.alter (const $ Just childRuntime) cId hsh, ()))
    where
        buildChildRuntime cId restartCountRef = do
          wCreationTime <- getCurrentTime
          wAsync <- async $ do
              eResult <- try (childSpecAction spec)
              restartCount <- atomicModifyIORef restartCountRef (\n -> (succ n, n))
              result <-
                  case eResult of
                      Left err ->
                          return $ ChildFailed (childSpecName spec) cId restartCount wCreationTime err
                      Right _ ->
                          return $ ChildCompleted (childSpecName spec) cId restartCount wCreationTime
              atomically
                  $ writeTQueue srSupEvQueue result

          return $ ChildProcess {
                childAsync = wAsync
              , childId = cId
              , childRestartCountRef = restartCountRef
              , childCreationTime = wCreationTime
              , childSpec = spec
              }

_startChild
  :: SupervisorRuntime
  -> ChildSpec
  -> IO ()
_startChild sRuntime spec =
    _forkChild sRuntime Nothing Nothing spec

startChild
  :: ChildSpec
  -> Supervisor
  -> IO ()
startChild spec sup =
  _startChild (supRuntime sup)
              spec

_restartChild
  :: SupervisorRuntime
  -> ChildId
  -> RestartCount
  -> ChildSpec
  -> IO ()
_restartChild sr cId restartCount =
    _forkChild sr (Just cId) (Just restartCount)

haltChild :: Text -> ChildId -> Supervisor -> IO ()
haltChild reason cId sup = do
    let
      (SupervisorRuntime {..}) = supRuntime sup

    mChild <- atomicModifyIORef srSupChildMap
                                (\hsh -> ( H.delete cId hsh
                                         , H.lookup cId hsh
                                         ))
    case mChild of
        Nothing -> do
            evTime <- getCurrentTime
            supNotifyEvent srSupSpec
              (ChildAlreadyHalted (supName srSupSpec)
                                  srSupId
                                  cId
                                  evTime)
            return ()
        Just child -> do
          evTime <- getCurrentTime
          supNotifyEvent srSupSpec
            (ChildTerminated (supName srSupSpec)
                             srSupId
                             (asyncThreadId (childAsync child))
                             (childId child)
                             (childSpecName (childSpec child))
                             evTime)
          cancelWith (childAsync child) (KillChild cId reason)

haltChildren :: Text -> SupervisorTerminationPolicy -> Supervisor -> IO ()
haltChildren reason policy sup = do
    let
      (SupervisorRuntime {..}) = supRuntime sup

    currentChildMap <-
        atomicModifyIORef srSupChildMap (\hsh -> (H.empty, hsh))

    let
      childs1 =
        sortBy (comparing childCreationTime) (H.elems currentChildMap)

      childs =
        case policy of
          OldestFirst ->
            childs1
          NewestFirst ->
            reverse childs1

    forM_ childs $ \child -> do
        -- kill synchronously from termination policy order
        haltChild reason (childId child) sup
        wait (childAsync child)


stopSupervisor :: Text -> SupervisorTerminationPolicy -> Supervisor -> IO ()
stopSupervisor reason terminationPolicy sup = do
    atomically $ writeTVar (srSupStatus (supRuntime sup)) SupHalting
    haltChildren reason terminationPolicy sup
    atomically $ writeTVar (srSupStatus (supRuntime sup)) SupHalted

readSupervisorStatus :: TVar SupervisorStatus -> STM SupervisorStatus
readSupervisorStatus statusRef = do
    status <- readTVar statusRef
    case status of
        SupInit ->
            retry
        _ -> do
            return status

removeChildFromMap
  :: ChildId
  -> IORef (HashMap ChildId ChildProcess)
  -> IO (Maybe ChildProcess)
removeChildFromMap cId cMap = do
  atomicModifyIORef
      cMap
      (\hsh -> ( H.delete cId hsh
               , H.lookup cId hsh
               ))

processChildEvent
  :: SupervisorRuntime
  -> ChildEvent
  -> ChildProcess
  -> IO ()
processChildEvent sr@(SupervisorRuntime {..}) ev child = do
    evTime <- getCurrentTime
    case ev of
        ChildCompleted _cName cId restartCount _startTime -> do
            case childRestartStrategy (childSpec child) of
                Permanent -> do
                    supNotifyEvent srSupSpec
                      (ChildRestarted (supName srSupSpec)
                                      srSupId
                                      cId
                                      (childSpecName (childSpec child))
                                      evTime
                                      ev)
                    _restartChild sr cId restartCount (childSpec child)
                _ -> do
                    supNotifyEvent srSupSpec
                      (ChildDropped (supName srSupSpec)
                                    srSupId
                                    (asyncThreadId (childAsync child))
                                    cId
                                    (childSpecName (childSpec child))
                                    evTime)
                    return ()

        ChildFailed _cName cId restartCount startTime err -> do
            case childRestartStrategy (childSpec child) of
                Temporal -> do
                    supNotifyEvent srSupSpec
                      (ChildDropped (supName srSupSpec)
                                    srSupId
                                    (asyncThreadId (childAsync child))
                                    cId
                                    (childSpecName (childSpec child))
                                    evTime)
                _ -> do
                    supNotifyEvent srSupSpec
                      (ChildRestarted (supName srSupSpec)
                                      srSupId
                                      cId
                                      (childSpecName (childSpec child))
                                      evTime
                                      ev)
                    -- TODO: Check if restart all with intensity and period
                    _restartChild sr cId restartCount (childSpec child)


        ChildKilled _cName cId restartCount _startTime -> do
            case childRestartStrategy (childSpec child) of
                Temporal ->
                    supNotifyEvent srSupSpec
                      (ChildDropped (supName srSupSpec)
                                    srSupId
                                    (asyncThreadId (childAsync child))
                                    cId
                                    (childSpecName (childSpec child))
                                    evTime)
                Transient -> do
                    wait (childAsync child)
                    supNotifyEvent srSupSpec
                      (ChildTerminated (supName srSupSpec)
                                       srSupId
                                       (asyncThreadId (childAsync child))
                                       cId
                                       (childSpecName (childSpec child))
                                       evTime)
                _ -> do
                    supNotifyEvent srSupSpec
                      (ChildRestarted (supName srSupSpec)
                                      srSupId
                                      cId
                                      (childSpecName (childSpec child))
                                      evTime
                                      ev)
                    _restartChild sr cId restartCount (childSpec child)

startSupervisor :: SupervisorSpec -> IO Supervisor
startSupervisor srSupSpec = do
    srSupId       <- UUID.nextRandom
    srSupStatus   <- newTVarIO SupInit
    srSupChildMap <- newIORef H.empty
    srSupEvQueue  <- newTQueueIO
    let
      supRuntime = SupervisorRuntime {..}

    supAsync    <- async $ do
      mapM_ (_startChild supRuntime) (supChildSpecList srSupSpec)
      childEventLoop supRuntime
    return $ Supervisor { .. }
  where
    childEventLoop
      :: SupervisorRuntime
      -> IO ()
    childEventLoop sr@(SupervisorRuntime {..}) = do
        (status, ev) <-
            atomically
              $ (,) <$> readSupervisorStatus srSupStatus
                    <*> readTQueue srSupEvQueue

        case status of
            SupInit -> do
                let
                  errMsg =
                    "Event Loop execution on inconsistent state; supervisor not initialized."
                supNotifyEvent srSupSpec (SupervisorStateError (supName srSupSpec) srSupId errMsg)
                panic errMsg

            SupHalted -> do
                -- Finish of supervisor loop
                -- TODO: Cleanup queue
                evTime <- getCurrentTime
                supNotifyEvent srSupSpec (SupervisorHalted (supName srSupSpec) srSupId evTime)
                return ()

            SupHalting ->
                -- ignore all till SupHalted
                childEventLoop sr

            SupRunning -> do
                -- normal behavior here
                mChild <- removeChildFromMap (ceChildId ev) srSupChildMap

                case mChild of
                    Nothing -> do
                      -- Child has been removed from known map, unlikely
                      -- event, need to trace why this would happen
                      evTime <- getCurrentTime
                      supNotifyEvent srSupSpec
                        (ChildAlreadyHalted (supName srSupSpec)
                                            srSupId
                                            (ceChildId ev)
                                            evTime)
                      return ()
                    Just child ->
                      processChildEvent sr ev child

                childEventLoop sr


otpRestartStrategy
  :: Int
  -> Int
  -> SomeException
  -> RestartCount
  -> StartTime
  -> UTCTime
  -> Maybe StartupDelay
otpRestartStrategy _intesityCount _periodMs _ _currentRestartCount _startTime _currentTime =
  undefined

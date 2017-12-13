{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | Example of a library file. It is also used for testing the test suites.
module Lib where

import Protolude hiding (try)

import Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime, diffUTCTime)

import Control.Exception.Safe (try)
import Data.Default           (Default (..))
import Data.HashMap.Strict    (HashMap)
import Data.IORef             (IORef, newIORef, atomicModifyIORef)

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
    { seSupervisorName :: !SupervisorName
    , seSupervisorId   :: !SupervisorId
    , seEventTime      :: !UTCTime }
  | SupervisorHalted
    { seSupervisorName :: !SupervisorName
    , seSupervisorId   :: !SupervisorId
    , seEventTime      :: !UTCTime }
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
  | ChildRestartCountReset
    { seSupervisorName :: !SupervisorName
    , seSupervisorId   :: !SupervisorId
    , seChildId        :: !ChildId
    , seChildThreadId  :: !ChildThreadId
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
      sfRestartStrategy :: !SupervisorRestartStrategy
    , sfIntensity       :: !Int
    , sfPeriodSeconds   :: !NominalDiffTime
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
type Intensity = Int
type PeriodSeconds = NominalDiffTime
type DiffSeconds = NominalDiffTime

data RestartAction
  = ResetRestartCount
  | IncreaseRestartCount
  | HaltRestart
  deriving (Generic, Show, Eq)

instance NFData RestartAction

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
      csName            :: !ChildName  -- ^ Child name (tracing purposes)
    , csRestartStrategy :: !ChildRestartStrategy -- ^ Restart strategy for child thread
    , csOnFailure       :: !(SomeException -> IO ()) -- ^ Callback executed on child failure
    , csOnCompleted     :: !(IO ()) -- ^ Callback executed on child successful exit
    , csAction          :: !(IO ()) -- ^ Child action to be monitored
    }
    deriving (Generic)

data ChildRuntime
  = ChildRuntime {
      crChildId              :: !ChildId
    , crChildCreationTime    :: !UTCTime
    , crChildSpec            :: !ChildSpec
    }
    deriving (Generic)

data ChildProcess
  = ChildProcess {
      cpAsync   :: !(Async ())
    , cpRuntime :: !ChildRuntime
    }
    deriving (Generic)

data SupervisorOptions
  = SupervisorOptions {
      ssName          :: !SupervisorName
    , ssRestartPolicy :: !SupervisorRestartStrategy
    , ssFlags         :: !SupervisorFlags
    , ssChildSpecList :: ![ChildSpec]
    , ssNotifyEvent   :: !(SupervisorEvent -> IO ())
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
      srSupervisorId    :: !SupervisorId
    , srEventQueue      :: !(TQueue ChildEvent)
    , srChildProcessMap :: !(IORef (HashMap ChildId ChildProcess))
    , srStatus          :: !(TVar SupervisorStatus)
    , srSupervisorSpec  :: !SupervisorOptions
    }
  deriving (Generic)

data Supervisor
  = Supervisor {
      supervisorAsync   :: !(Async ())
    , supervisorRuntime :: !SupervisorRuntime
    }
  deriving (Generic)

data ChildException
  = KillChild {
      exChildId    :: !ChildId
    , exKillReason :: !Text
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
  -> IO ChildId
_forkChild (SupervisorRuntime {..}) mChildId mChildRestartCount crChildSpec = do
        crChildId              <- maybe UUID.nextRandom return mChildId
        cpRuntime              <- buildChildRuntime crChildId

        atomicModifyIORef srChildProcessMap
                          (\m -> ( H.alter (const $ Just cpRuntime) crChildId m
                                 , ()
                                 ))
        return crChildId
    where
        restartCount =
          fromMaybe 0 mChildRestartCount

        buildChildRuntime crChildId = do
          let
            ChildSpec {..} =
              crChildSpec

          crChildCreationTime <- getCurrentTime
          cpAsync <- async $ do
              eResult <- try csAction
              result <-
                  case eResult of
                      Left err ->
                          return
                            $ ChildFailed csName crChildId (succ restartCount) crChildCreationTime err
                      Right _ ->
                          return $ ChildCompleted csName crChildId restartCount crChildCreationTime
              atomically
                  $ writeTQueue srEventQueue result

          let
            cpRuntime =
                ChildRuntime {..}

          return $ ChildProcess {..}

_startChild
  :: SupervisorRuntime
  -> ChildSpec
  -> IO ChildId
_startChild supervisorRuntime childSpec =
    _forkChild supervisorRuntime Nothing Nothing childSpec

_restartChild
  :: SupervisorRuntime
  -> ChildId
  -> RestartCount
  -> ChildSpec
  -> IO ChildId
_restartChild sr childId childRestartCount =
    _forkChild sr (Just childId) (Just childRestartCount)

_restartChildren
  :: RestartCount
  -> SupervisorRuntime
  -> SupervisorTerminationPolicy
  -> IO ()
_restartChildren restartCount sr@(SupervisorRuntime {..}) terminationPolicy = do
    childMap <- atomicModifyIORef srChildProcessMap (\m -> (H.empty, m))

    let
      sortedChildren =
        sortBy (comparing (crChildCreationTime . cpRuntime)) (H.elems childMap)

      children =
        case terminationPolicy of
          OldestFirst ->
            sortedChildren

          NewestFirst ->
            reverse sortedChildren

    forM_ children $ \child -> do
      let
        ChildRuntime {..} =
          cpRuntime child

        ChildSpec {..} =
          crChildSpec

      _restartChild sr crChildId restartCount crChildSpec

_haltChildren :: Text -> SupervisorTerminationPolicy -> Supervisor -> IO ()
_haltChildren reason policy sup = do
    let
        SupervisorRuntime {..} =
            supervisorRuntime sup

    currentChildMap <-
        atomicModifyIORef srChildProcessMap (\hsh -> (H.empty, hsh))

    let
        sortedChildren =
            sortBy (comparing (crChildCreationTime . cpRuntime))
                   (H.elems currentChildMap)

        children =
            case policy of
                OldestFirst ->
                    sortedChildren
                NewestFirst ->
                    reverse sortedChildren

    forM_ children $ \child -> do
        let
            ChildRuntime {..} =
                cpRuntime child

        -- kill synchronously from termination policy order
        haltChild reason crChildId sup
        wait (cpAsync child)

_readSupervisorStatus :: TVar SupervisorStatus -> STM SupervisorStatus
_readSupervisorStatus statusRef = do
    status <- readTVar statusRef
    case status of
        SupInit ->
            retry
        _ -> do
            return status

_removeChildFromMap
  :: ChildId
  -> IORef (HashMap ChildId ChildProcess)
  -> IO (Maybe ChildProcess)
_removeChildFromMap childId cMap = do
 atomicModifyIORef
      cMap
      (\hsh -> ( H.delete childId hsh
               , H.lookup childId hsh
               ))

_withChildProcess
  :: SupervisorRuntime
  -> ChildId
  -> (ChildProcess -> IO ())
  -> IO ()
_withChildProcess (SupervisorRuntime {..}) childId actionFn = do
    let
      SupervisorOptions {..} =
        srSupervisorSpec

    mChild <- _removeChildFromMap childId srChildProcessMap

    case mChild of
        Nothing -> do
          -- Child has been removed from known map, unlikely
          -- event, need to trace why this would happen
          eventTime <- getCurrentTime
          ssNotifyEvent
            (ChildAlreadyHalted ssName
                                srSupervisorId
                                childId
                                eventTime)
          return ()
        Just child ->
          actionFn child

getDiffSeconds :: MonadIO m => ChildEvent -> m NominalDiffTime
getDiffSeconds ev = do
  currentTime <- liftIO getCurrentTime
  return $ diffUTCTime currentTime (ceStartTime ev)

_getRestartAction
  :: Intensity
  -> PeriodSeconds
  -> RestartCount
  -> DiffSeconds
  -> RestartAction
_getRestartAction intensity periodSeconds restartCount diffSeconds =
  case () of
    _ | diffSeconds <= periodSeconds &&
          restartCount >= intensity ->
        HaltRestart
      | diffSeconds >= periodSeconds ->
        ResetRestartCount
      | otherwise ->
        IncreaseRestartCount

__invokeRestartPolicy
  :: SupervisorRuntime
  -> ChildId
  -> ChildSpec
  -> RestartCount
  -> SupervisorRestartStrategy
  -> IO ()
__invokeRestartPolicy sr childId childSpec restartCount policy =
  case policy of
      AllForOne terminationPolicy ->
          _restartChildren restartCount sr terminationPolicy

      OneForOne ->
          void $ _restartChild sr childId 1 childSpec


_invokeRestartPolicy
  :: SupervisorRuntime
  -> ChildProcess
  -> ChildEvent
  -> IO ()
_invokeRestartPolicy sr@(SupervisorRuntime {..}) (ChildProcess {..}) ev = do
    eventTime <- getCurrentTime

    let
      SupervisorOptions {..} =
        srSupervisorSpec

      SupervisorFlags {..} =
        ssFlags

      ChildRuntime {..} =
        cpRuntime

      ChildSpec {..} =
        crChildSpec

      restartCount =
        ceRestartCount ev

    ssNotifyEvent
      (ChildRestarted ssName
                      srSupervisorId
                      crChildId
                      csName
                      eventTime
                      ev)

    -- Validate intensity and period to force a halt if necessary
    restartAction <- _getRestartAction sfIntensity sfPeriodSeconds restartCount <$> (getDiffSeconds ev)

    case restartAction of
      HaltRestart ->
        panic "pending"

      ResetRestartCount -> do
        evTime <- getCurrentTime
        ssNotifyEvent
          (ChildRestartCountReset ssName
                                  srSupervisorId
                                  crChildId
                                  (asyncThreadId cpAsync)
                                  csName
                                  evTime)
        __invokeRestartPolicy sr crChildId crChildSpec 1 ssRestartPolicy

      IncreaseRestartCount ->
        __invokeRestartPolicy sr crChildId crChildSpec (succ restartCount) ssRestartPolicy

_processChildEvent
  :: SupervisorRuntime
  -> ChildEvent
  -> ChildProcess
  -> IO ()
_processChildEvent sr@(SupervisorRuntime {..}) ev child = do
    eventTime <- getCurrentTime
    let
      SupervisorOptions {..} =
        srSupervisorSpec

      ChildRuntime {..} =
        cpRuntime child

      ChildSpec {..} =
        crChildSpec

    case ev of
        ChildCompleted {} -> do
            case csRestartStrategy of
                Permanent ->
                    _invokeRestartPolicy sr child ev

                _ ->
                    ssNotifyEvent
                      (ChildDropped ssName
                                    srSupervisorId
                                    (asyncThreadId (cpAsync child))
                                    (ceChildId ev)
                                    (ceChildName ev)
                                    eventTime)

        ChildFailed {} -> do
            case csRestartStrategy of
                Temporal -> do
                    ssNotifyEvent
                      (ChildDropped ssName
                                    srSupervisorId
                                    (asyncThreadId (cpAsync child))
                                    (ceChildId ev)
                                    (ceChildName ev)
                                    eventTime)
                _ -> do
                    _invokeRestartPolicy sr child ev


        ChildKilled {} -> do
            case csRestartStrategy of
                Temporal ->
                    ssNotifyEvent
                      (ChildDropped ssName
                                    srSupervisorId
                                    (asyncThreadId (cpAsync child))
                                    (ceChildId ev)
                                    (ceChildName ev)
                                    eventTime)
                Transient -> do
                    wait (cpAsync child)
                    ssNotifyEvent
                      (ChildTerminated ssName
                                       srSupervisorId
                                       (asyncThreadId (cpAsync child))
                                       (ceChildId ev)
                                       (ceChildName ev)
                                       eventTime)
                _ ->
                    _invokeRestartPolicy sr child ev

--------------------------------------------------------------------------------

startChild :: ChildSpec -> Supervisor -> IO ChildId
startChild childSpec supervisor =
    _startChild (supervisorRuntime supervisor)
                childSpec

haltChild :: Text -> ChildId -> Supervisor -> IO ()
haltChild reason childId sup = do
    let
        sr@(SupervisorRuntime {..}) =
            supervisorRuntime sup

    _withChildProcess sr childId $ \child -> do
        eventTime <- getCurrentTime

        let
            SupervisorOptions {..} =
              srSupervisorSpec

            ChildProcess {..} =
                child

            ChildRuntime {..} =
                cpRuntime

            ChildSpec {..} =
                crChildSpec

        ssNotifyEvent
            (ChildTerminated ssName
                             srSupervisorId
                             (asyncThreadId cpAsync)
                             crChildId
                             csName
                             eventTime)

        cancelWith cpAsync (KillChild childId reason)

stopSupervisor
  :: Text
  -> SupervisorTerminationPolicy
  -> Supervisor
  -> IO ()
stopSupervisor reason terminationPolicy sup = do

    let
      SupervisorRuntime {..} =
        supervisorRuntime sup

    atomically $ writeTVar srStatus SupHalting
    _haltChildren reason terminationPolicy sup
    atomically $ writeTVar srStatus SupHalted

startSupervisor :: SupervisorOptions -> IO Supervisor
startSupervisor spec = do
    srSupervisorId    <- UUID.nextRandom
    srStatus          <- newTVarIO SupInit
    srChildProcessMap <- newIORef H.empty
    srEventQueue      <- newTQueueIO

    let
      supervisorRuntime = SupervisorRuntime {srSupervisorSpec = spec, ..}

    supervisorAsync    <- async $ do
      mapM_ (_startChild supervisorRuntime) (ssChildSpecList spec)
      childEventLoop supervisorRuntime

    return $ Supervisor { .. }
  where
    childEventLoop
      :: SupervisorRuntime
      -> IO ()
    childEventLoop sr@(SupervisorRuntime {..}) = do
        let
          SupervisorOptions {..} =
            srSupervisorSpec

        (status, ev) <-
            atomically
              $ (,) <$> _readSupervisorStatus srStatus
                    <*> readTQueue srEventQueue

        case status of
            SupInit -> do
                let
                  errMsg =
                    "Event Loop execution on inconsistent state; supervisor not initialized."
                ssNotifyEvent (SupervisorStateError ssName srSupervisorId errMsg)
                panic errMsg

            SupHalted -> do
                -- Finish of supervisor loop
                -- TODO: Cleanup queue
                eventTime <- getCurrentTime
                ssNotifyEvent (SupervisorHalted ssName srSupervisorId eventTime)
                return ()

            SupHalting ->
                -- ignore all till SupHalted
                childEventLoop sr

            SupRunning -> do
                -- normal behavior here
                _withChildProcess sr (ceChildId ev) (_processChildEvent sr ev)
                childEventLoop sr

-- otpRestartStrategy
--   :: Int
--   -> Int
--   -> SomeException
--   -> RestartCount
--   -> StartTime
--   -> UTCTime
--   -> Maybe StartupDelay
-- otpRestartStrategy _intesityCount _periodMs _ _currentRestartCount _startTime _currentTime =
--   undefined

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Util where

import Protolude

import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Data.IORef                    (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Test.Tasty.HUnit              (assertBool, assertFailure)
import Text.Show.Pretty              (ppShow)

import qualified Data.Text as T

import qualified Control.Concurrent.Capataz       as SUT
import qualified Control.Concurrent.Capataz.Event as SUT

--------------------------------------------------------------------------------
-- Util

-- | Utility function that gets the type name of a Record through it's Show
-- output.
fetchRecordName :: Show a => a -> Text
fetchRecordName = T.takeWhile (/= ' ') . show

-- | Composes two predicate functions together with a boolean AND
andP :: [a -> Bool] -> a -> Bool
andP predList a = all ($ a) predList

--------------------------------------------------------------------------------
-- Assertions and Testers

-- | This record duplicate the same event names as the ones found in the
-- "CapatazEvent" type, we use this to avoid using Text comparisons on assertion
-- helper functions. The "CapatazEvent" record is imported qualified, so there
-- is no conflict happening.
data EventType
  = InvalidCapatazStatusReached
  | SupervisorStatusChanged
  | ProcessTerminated
  | ProcessStarted
  | ProcessRestarted
  | ProcessCompleted
  | ProcessFailed
  | ProcessCallbackExecuted
  | ProcessTerminationStarted
  | ProcessTerminationFinished
  | CapatazFailed
  | CapatazTerminated
  deriving (Show, Eq)

toEventType :: SUT.CapatazEvent -> EventType
toEventType ev = case ev of
  SUT.InvalidSupervisorStatusReached{} -> InvalidCapatazStatusReached
  SUT.SupervisorStatusChanged{}        -> SupervisorStatusChanged
  SUT.ProcessTerminated{}              -> ProcessTerminated
  SUT.ProcessStarted{}                 -> ProcessStarted
  SUT.ProcessRestarted{}               -> ProcessRestarted
  SUT.ProcessCompleted{}               -> ProcessCompleted
  SUT.ProcessFailed{}                  -> ProcessFailed
  SUT.ProcessCallbackExecuted{}        -> ProcessCallbackExecuted
  SUT.ProcessTerminationStarted{}      -> ProcessTerminationStarted
  SUT.ProcessTerminationFinished{}     -> ProcessTerminationFinished
  SUT.CapatazFailed{}                  -> CapatazFailed
  SUT.CapatazTerminated{}              -> CapatazTerminated

-- | Predicate function to assert "CapatazEvent" types
assertEventType :: EventType -> SUT.CapatazEvent -> Bool
assertEventType evType ev = toEventType ev == evType

-- | Predicate function to assert "CapatazEvent" process type
assertProcessType :: SUT.ProcessType -> SUT.CapatazEvent -> Bool
assertProcessType processTy ev = case ev of
  SUT.ProcessFailed { processType }           -> processTy == processType
  SUT.ProcessTerminated { processType }       -> processTy == processType
  SUT.ProcessStarted { processType }          -> processTy == processType
  SUT.ProcessCallbackExecuted { processType } -> processTy == processType
  SUT.ProcessRestarted { processType }        -> processTy == processType
  _                                           -> False

-- | Predicate function to assert "CapatazEvent" worker name
assertProcessName :: Text -> SUT.CapatazEvent -> Bool
assertProcessName processName' ev = case ev of
  SUT.ProcessRestarted { processName }  -> processName' == processName
  SUT.ProcessFailed { processName }     -> processName' == processName
  SUT.ProcessTerminated { processName } -> processName' == processName
  SUT.ProcessStarted { processName }    -> processName' == processName
  _                                     -> False

-- | Predicate function to assert type of an error inside a "CapatazEvent"
assertErrorType :: Text -> SUT.CapatazEvent -> Bool
assertErrorType errType ev = case ev of
  SUT.CapatazFailed { supervisorError } ->
    fetchRecordName supervisorError == errType
  SUT.ProcessFailed { processError } -> fetchRecordName processError == errType
  SUT.ProcessCallbackExecuted { processCallbackError } ->
    case processCallbackError of
      Nothing            -> False
      Just originalError -> fetchRecordName originalError == errType
  _ -> False

-- | Predicate function to assert type of callback executed inside a
-- "CapatazEvent"
assertCallbackType :: SUT.CallbackType -> SUT.CapatazEvent -> Bool
assertCallbackType cbType ev = case ev of
  SUT.ProcessFailed { processError } -> case fromException processError of
    Just SUT.ProcessCallbackFailed { processCallbackType } ->
      cbType == processCallbackType
    _ -> False
  SUT.ProcessCallbackExecuted { processCallbackType } ->
    cbType == processCallbackType
  _ -> False

-- | Predicate function to assert restart count inside a "CapatazEvent"
assertRestartCount :: (Int -> Bool) -> SUT.CapatazEvent -> Bool
assertRestartCount predFn ev = case ev of
  SUT.ProcessRestarted { processRestartCount } -> predFn processRestartCount
  _                                            -> False

-- | Predicate function to assert a Capataz status change
assertSupervisorStatusChanged
  :: SUT.SupervisorStatus -> SUT.SupervisorStatus -> SUT.CapatazEvent -> Bool
assertSupervisorStatusChanged fromEv toEv ev = case ev of
  SUT.SupervisorStatusChanged { prevSupervisorStatus, newSupervisorStatus } ->
    fromEv == prevSupervisorStatus && toEv == newSupervisorStatus
  _ -> False

-- | Predicate function to assert process was supervised by a a supervisor
-- indicated by name
assertSupervisorName :: Text -> SUT.CapatazEvent -> Bool
assertSupervisorName supervisorName' ev = case ev of
  SUT.SupervisorStatusChanged { supervisorName } ->
    supervisorName' == supervisorName
  SUT.ProcessStarted { supervisorName }    -> supervisorName' == supervisorName
  SUT.ProcessTerminated { supervisorName } -> supervisorName' == supervisorName
  SUT.ProcessRestarted { supervisorName }  -> supervisorName' == supervisorName
  SUT.ProcessCompleted { supervisorName }  -> supervisorName' == supervisorName
  SUT.ProcessCallbackExecuted { supervisorName } ->
    supervisorName' == supervisorName
  SUT.ProcessTerminationStarted { supervisorName } ->
    supervisorName' == supervisorName
  SUT.ProcessTerminationFinished { supervisorName } ->
    supervisorName' == supervisorName
  _ -> False

-- | Predicate function to assert a supervisor was started
assertSupervisorStarted :: Text -> SUT.CapatazEvent -> Bool
assertSupervisorStarted supervisorName = andP
  [ assertEventType ProcessStarted
  , assertProcessType SUT.SupervisorType
  , assertProcessName supervisorName
  ]

-- | Predicate function to assert a supervisor was terminated
assertSupervisorTerminated :: Text -> SUT.CapatazEvent -> Bool
assertSupervisorTerminated supervisorName = andP
  [ assertEventType ProcessTerminated
  , assertProcessType SUT.SupervisorType
  , assertProcessName supervisorName
  ]

assertSupervisorFailed :: Text -> SUT.CapatazEvent -> Bool
assertSupervisorFailed supervisorName = andP
  [ assertEventType ProcessFailed
  , assertProcessType SUT.SupervisorType
  , assertProcessName supervisorName
  ]

assertSupervisorRestarted :: Text -> SUT.CapatazEvent -> Bool
assertSupervisorRestarted supervisorName = andP
  [ assertEventType ProcessRestarted
  , assertProcessType SUT.SupervisorType
  , assertProcessName supervisorName
  ]

-- | Predicate function to assert a worker was started
assertWorkerStarted :: Text -> SUT.CapatazEvent -> Bool
assertWorkerStarted workerName = andP
  [ assertEventType ProcessStarted
  , assertProcessType SUT.WorkerType
  , assertProcessName workerName
  ]

-- | Predicate function to assert a worker was terminated
assertWorkerTerminated :: Text -> SUT.CapatazEvent -> Bool
assertWorkerTerminated workerName = andP
  [ assertEventType ProcessTerminated
  , assertProcessType SUT.WorkerType
  , assertProcessName workerName
  ]

assertWorkerFailed :: Text -> SUT.CapatazEvent -> Bool
assertWorkerFailed workerName = andP
  [ assertEventType ProcessFailed
  , assertProcessType SUT.WorkerType
  , assertProcessName workerName
  ]

assertWorkerRestarted :: Text -> SUT.CapatazEvent -> Bool
assertWorkerRestarted workerName = andP
  [ assertEventType ProcessRestarted
  , assertProcessType SUT.WorkerType
  , assertProcessName workerName
  ]

-- | Predicate function to assert a capataz thread failed with error type
assertCapatazFailedWith :: Text -> SUT.CapatazEvent -> Bool
assertCapatazFailedWith errorName =
  andP [assertEventType CapatazFailed, assertErrorType errorName]

--------------------------------------------------------------------------------

-- | Exception used to test failures inside Worker sub-routines
data RestartingWorkerError
  = RestartingWorkerError
  deriving (Show)

instance Exception RestartingWorkerError

-- | Exception used to test failures inside Worker callback sub-routines
data TimeoutError
  = TimeoutError
  deriving (Show)

instance Exception TimeoutError

-- | Utility function to create a Worker sub-routine that fails at least a
-- number of times
mkFailingSubRoutine
  :: Int  -- ^ Number of times the Worker sub-routine will fail
  -> IO (IO ()) -- ^ Sub-routine used on worker creation
mkFailingSubRoutine failCount = do
  countRef <- newIORef failCount
  let subRoutine = do
        shouldFail <- atomicModifyIORef' countRef
                                         (\count -> (pred count, count > 0))
        when shouldFail (throwIO RestartingWorkerError)

  return subRoutine

-- | A sub-routine that will complete for `initCount` amount of times. This
-- function works great when testing `Permanent` strategies, as you would like
-- to assert restart events once (if it keeps completing it will fill up the log
-- with restart events)
mkCompletingBeforeNRestartsSubRoutine :: Int -> IO (IO ())
mkCompletingBeforeNRestartsSubRoutine initCount = do
  countRef <- newIORef initCount
  let subRoutine = do
        shouldStop <- atomicModifyIORef' countRef
                                         (\count -> (pred count, count > 0))
        if shouldStop then return () else forever $ threadDelay 1000100
  return subRoutine

-- | A sub-routine that will complete once. This function works great when
-- testing `Permanent` strategies, as you would like to assert restart events
-- once (if it keeps completing it will fill up the log with restart events)
mkCompletingOnceSubRoutine :: IO (IO ())
mkCompletingOnceSubRoutine = mkCompletingBeforeNRestartsSubRoutine 1

-- | Utility function to build a test environment for a Capataz execution.
-- It is composed by:
--
-- * List of assertions that represent events that should be triggered by the
--   capataz instance in order
--
-- * A function to modify the default "CapatazOptions", this utility function injects
--   a special @notifyEvent@ callback to execute given assertions.
testCapatazStreamWithOptions
  :: (SUT.CapatazOptions -> SUT.CapatazOptions) -- ^ Function to modify default
                                                -- @CapatazOptions@
  -> [SUT.CapatazEvent -> Bool] -- ^ Assertions happening before setup function
                                -- is called
  -> (SUT.Capataz -> IO ()) -- ^ Function used to test public the supervisor
                            -- public API (a.k.a setup function)
  -> [SUT.CapatazEvent -> Bool] -- ^ Assertions happening after the setup
                                -- function
  -> [SUT.CapatazEvent -> Bool] -- ^ Assertions happening after the capataz
                                -- record is terminated
  -> Maybe (SUT.CapatazEvent -> Bool) -- ^ An assertion checked across all
                                      -- @CapatazEvents@ that happened in a
                                      -- test, great when testing that an event
                                      -- __did not__ happen
  -> IO ()
testCapatazStreamWithOptions optionModFn preSetupAssertion setupFn postSetupAssertions postTeardownAssertions mAllEventsAssertion
  = do

    eventStream     <- newTQueueIO
    accRef          <- newIORef []
    pendingCountVar <- newIORef
      ( sum $ fmap
        length
        [preSetupAssertion, postSetupAssertions, postTeardownAssertions]
      )

    capataz <- SUT.forkCapataz
      (SUT.set SUT.onSystemEventL (trackEvent accRef eventStream) . optionModFn)

    -- We check preSetup assertions are met before we execute the setup
    -- function. This serves to test initialization of capataz instance
    runAssertions "PRE-SETUP"
                  (eventStream, accRef)
                  pendingCountVar
                  preSetupAssertion
                  capataz

    -- We execute the setup sub-routine, which is going to use the Capataz public
    -- API to assert events
    setupResult <- try (setupFn capataz)

    case setupResult of
      -- If the sub-routine fails, show exception
      Left  err -> assertFailure (show (err :: SomeException))
      Right _   -> do
        -- We now run post-setup assertions
        runAssertions "POST-SETUP"
                      (eventStream, accRef)
                      pendingCountVar
                      postSetupAssertions
                      capataz

        -- We now shutdown the capataz instance
        void $ SUT.teardown capataz

        -- We run assertions for after the capataz has been shut down
        runAssertions "POST-TEARDOWN"
                      (eventStream, accRef)
                      pendingCountVar
                      postTeardownAssertions
                      capataz

        -- Lastly, we check if there is a function that we want to execute
        -- across all events that happened in the test, this is to assert the
        -- absence of an event
        case mAllEventsAssertion of
          Nothing                 -> return ()
          Just allEventsAssertion -> do
            events <- reverse <$> readIORef accRef
            assertBool
              ( "On AFTER-TEST, expected all events to match predicate, but didn't ("
              <> show (length events)
              <> " events tried)\n"
              <> ppShow (zip ([0 ..] :: [Int]) events)
              )
              (all allEventsAssertion events)
 where
  -- Utility functions that runs the readEventLoop function with a timeout
  -- of a second, this way we can guarantee assertions are met without having
  -- to add @threadDelays@ to the test execution
  runAssertions stageName (eventStream, accRef) pendingCountVar assertions capataz
    = do
      raceResult <- race
        (threadDelay 1000100)
        (readEventLoop eventStream pendingCountVar assertions)
      case raceResult of
        Left _ -> do
          events       <- reverse <$> readIORef accRef
          pendingCount <- readIORef pendingCountVar
          void $ SUT.teardown capataz
          assertFailure
            (  "On "
            <> stageName
            <> " stage, expected all assertions to match, but didn't ("
            <> show pendingCount
            <> " assertions remaining, "
            <> show (length events)
            <> " events tried)\n"
            <> ppShow (zip ([0 ..] :: [Int]) events)
            )
        Right _ -> return ()


  -- Sub-routine that accumulates all events that have happened in the Capataz
  -- instance so far
  trackEvent accRef eventStream event = do
    atomicModifyIORef' accRef (\old -> (event : old, ()))
    atomically $ writeTQueue eventStream event

  -- Sub-routine that reads the event stream, and ensures that all assertions
  -- are executed, this loop won't stop until all assertions are met
  readEventLoop eventStream pendingCount assertions = do
    writeIORef pendingCount (length assertions)
    case assertions of
      []                        -> return ()
      (assertionFn:assertions1) -> do
        event <- atomically $ readTQueue eventStream
        if assertionFn event
          then readEventLoop eventStream pendingCount assertions1
          else readEventLoop eventStream pendingCount assertions


-- | A version of "testCapatazStreamWithOptions" that does not receive the
-- function that modifies a "CapatazOptions" record.
testCapatazStream
  :: [SUT.CapatazEvent -> Bool] -- ^ Assertions happening before setup function
                                -- is called
  -> (SUT.Capataz -> IO ()) -- ^ Function used to test public the supervisor
                            -- public API (a.k.a setup function)
  -> [SUT.CapatazEvent -> Bool] -- ^ Assertions happening after the setup
                                -- function
  -> [SUT.CapatazEvent -> Bool] -- ^ Assertions happening after the capataz
                                -- record is terminated
  -> Maybe (SUT.CapatazEvent -> Bool) -- ^ An assertion checked across all
                                      -- @CapatazEvents@ that happened in a
                                      -- test, great when testing that an event
                                      -- __did not__ happen
  -> IO ()
testCapatazStream preSetupAssertions =
  testCapatazStreamWithOptions identity preSetupAssertions

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-| This module contains:

* Assertion functions to get attributes from a `CapatazEvent`

* Helpers to run the test (reduce boilerplate)

* Actual tests

Tests just exercises the __public API__ and asserts all the events delivered via
the @notifyEvent@ callback are what we are expecting.

NOTE: This tests may be flaky depending on the load of the application, there is
a ticket pending to add dejafu tests to ensure our tests are stable.

-}
module Control.Concurrent.CapatazTest (tests) where

import Protolude

import qualified Data.Text as T

import Data.IORef       (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Text.Show.Pretty (ppShow)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar   (modifyTVar', newTVarIO, readTVar)

import qualified Control.Concurrent.Capataz as SUT

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
  deriving (Show)

-- | Predicate function to assert "CapatazEvent" types
assertEventType :: EventType -> SUT.CapatazEvent -> Bool
assertEventType evType ev = fetchRecordName ev == show evType

-- | Predicate function to assert "CapatazEvent" worker name
assertWorkerName :: Text -> SUT.CapatazEvent -> Bool
assertWorkerName workerName' ev = case ev of
  SUT.ProcessRestarted { processName }  -> workerName' == processName
  SUT.ProcessFailed { processName }     -> workerName' == processName
  SUT.ProcessTerminated { processName } -> workerName' == processName
  SUT.ProcessStarted { processName }    -> workerName' == processName
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
  :: SUT.CapatazStatus -> SUT.CapatazStatus -> SUT.CapatazEvent -> Bool
assertSupervisorStatusChanged fromEv toEv ev = case ev of
  SUT.SupervisorStatusChanged { prevSupervisorStatus, newSupervisorStatus } ->
    fromEv == prevSupervisorStatus && toEv == newSupervisorStatus
  _ -> False

-- | Predicate function to assert a worker was started
assertWorkerStarted :: Text -> SUT.CapatazEvent -> Bool
assertWorkerStarted workerName =
  andP [assertEventType ProcessStarted, assertWorkerName workerName]

-- | Predicate function to assert a worker was terminated
assertWorkerTerminated :: Text -> SUT.CapatazEvent -> Bool
assertWorkerTerminated workerName =
  andP [assertEventType ProcessTerminated, assertWorkerName workerName]

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
  :: [SUT.CapatazEvent -> Bool] -- ^ Assertions happening before setup function
                                -- is called
  -> (SUT.CapatazOptions -> SUT.CapatazOptions) -- ^ Function to modify default
                                                -- @CapatazOptions@
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
testCapatazStreamWithOptions preSetupAssertion optionModFn setupFn postSetupAssertions postTeardownAssertions mAllEventsAssertion
  = do

    eventStream     <- newTQueueIO
    accRef          <- newIORef []
    pendingCountVar <- newIORef
      ( sum $ fmap
        length
        [preSetupAssertion, postSetupAssertions, postTeardownAssertions]
      )

    capataz <- SUT.forkCapataz $ (optionModFn SUT.defCapatazOptions)
      { SUT.notifyEvent = trackEvent accRef eventStream
      }

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
  testCapatazStreamWithOptions preSetupAssertions identity

--------------------------------------------------------------------------------
-- Actual Tests

tests :: [TestTree]
tests
  = [ testGroup
      "capataz without workerSpecList"
      [ testCase "initialize and teardown works as expected" $ testCapatazStream
          [ andP
              [ assertEventType SupervisorStatusChanged
              , assertSupervisorStatusChanged SUT.Initializing SUT.Running
              ]
          ]
          (const $ return ())
          []
          [ andP
            [ assertEventType SupervisorStatusChanged
            , assertSupervisorStatusChanged SUT.Running SUT.Halting
            ]
          , andP
            [ assertEventType SupervisorStatusChanged
            , assertSupervisorStatusChanged SUT.Halting SUT.Halted
            ]
          ]
          Nothing
      ]
    , testGroup
      "capataz with workerSpecList"
      [ testCase "initialize and teardown works as expected"
          $ testCapatazStreamWithOptions
              [ assertWorkerStarted "A"
              , assertWorkerStarted "B"
              , andP
                [ assertEventType SupervisorStatusChanged
                , assertSupervisorStatusChanged SUT.Initializing SUT.Running
                ]
              ]
              ( \supOptions -> supOptions
                { SUT.supervisorProcessSpecList = [ SUT.WorkerProcessSpec
                                                    $ SUT.defWorkerSpec
                                                        { SUT.workerName = "A"
                                                        , SUT.workerAction = forever
                                                          (threadDelay 10001000)
                                                        }
                                                  , SUT.WorkerProcessSpec
                                                    $ SUT.defWorkerSpec
                                                        { SUT.workerName = "B"
                                                        , SUT.workerAction = forever
                                                          (threadDelay 10001000)
                                                        }
                                                  ]
                }
              )
              (const $ return ())
              []
              [ andP
                [ assertEventType SupervisorStatusChanged
                , assertSupervisorStatusChanged SUT.Running SUT.Halting
                ]
              , assertEventType ProcessTerminationStarted
              , assertWorkerTerminated "A"
              , assertWorkerTerminated "B"
              , assertEventType ProcessTerminationFinished
              , andP
                [ assertEventType SupervisorStatusChanged
                , assertSupervisorStatusChanged SUT.Halting SUT.Halted
                ]
              ]
              Nothing
      ]
    , testCase "reports error when capataz thread receives async exception"
      $ testCapatazStream
          [ andP
              [ assertEventType SupervisorStatusChanged
              , assertSupervisorStatusChanged SUT.Initializing SUT.Running
              ]
          ]
          ( \capataz -> cancelWith (SUT.capatazToAsync capataz)
                                   (ErrorCall "async exception")
          )
          [assertEventType CapatazFailed]
          []
          Nothing
    , testCase "reports error when worker retries violate restart intensity"
      $ do
          lockVar <- newEmptyMVar
          let (signalIntensityReached, waitTillIntensityReached) =
                (putMVar lockVar (), takeMVar lockVar)
          testCapatazStreamWithOptions
            []
            ( \supOptions -> supOptions
              { SUT.supervisorOnIntensityReached = signalIntensityReached
              }
            )
            ( \capataz -> do
              _workerId <- SUT.forkWorker SUT.defWorkerOptions
                                          (throwIO RestartingWorkerError)
                                          capataz
              waitTillIntensityReached
            )
            [ assertEventType ProcessFailed
            , assertEventType ProcessFailed
            , assertEventType ProcessFailed
            , assertCapatazFailedWith "CapatazIntensityReached"
            ]
            []
            Nothing
    , testGroup
      "single supervised IO sub-routine"
      [ testGroup
        "callbacks"
        [ testGroup
          "workerOnCompletion"
          [ testCase "does execute callback when sub-routine is completed"
            $ testCapatazStream
                []
                ( \capataz -> do
                  _workerId <- SUT.forkWorker
                    ( SUT.defWorkerOptions
                      { SUT.workerRestartStrategy = SUT.Temporary
                      }
                    )
                    (return ())
                    capataz
                  return ()
                )
                [ andP
                  [ assertEventType ProcessCallbackExecuted
                  , assertCallbackType SUT.OnCompletion
                  ]
                , assertEventType ProcessCompleted
                ]
                []
                Nothing
          , testCase "does not execute callback when sub-routine fails"
            $ testCapatazStream
                []
                ( \capataz -> do
                  _workerId <- SUT.forkWorker
                    ( SUT.defWorkerOptions
                      { SUT.workerRestartStrategy = SUT.Temporary
                      }
                    )
                    (throwIO RestartingWorkerError)
                    capataz
                  return ()
                )
                [ andP
                  [ assertEventType ProcessCallbackExecuted
                  , assertCallbackType SUT.OnFailure
                  ]
                , assertEventType ProcessFailed
                ]
                [assertEventType CapatazTerminated]
                ( Just $ not . andP
                  [ assertEventType ProcessCallbackExecuted
                  , assertCallbackType SUT.OnCompletion
                  ]
                )
          , testCase "does not execute callback when sub-routine is terminated"
            $ testCapatazStream
                []
                ( \capataz -> do
                  workerId <- SUT.forkWorker
                    ( SUT.defWorkerOptions
                      { SUT.workerRestartStrategy = SUT.Temporary
                      }
                    )
                    (forever $ threadDelay 1000100)
                    capataz

                  _workerId <- SUT.terminateProcess
                    "testing onCompletion callback"
                    workerId
                    capataz
                  return ()
                )
                [assertEventType ProcessTerminated]
                [assertEventType CapatazTerminated]
                ( Just $ not . andP
                  [ assertEventType ProcessCallbackExecuted
                  , assertCallbackType SUT.OnCompletion
                  ]
                )
          , testCase "treats as sub-routine failed if callback fails"
            $ testCapatazStream
                []
                ( \capataz -> do
                  _workerId <- SUT.forkWorker
                    ( SUT.defWorkerOptions
                      { SUT.workerRestartStrategy = SUT.Temporary
                      , SUT.workerOnCompletion    = throwIO TimeoutError
                      }
                    )
                    (return ())
                    capataz

                  return ()
                )
                [ andP
                  [ assertEventType ProcessCallbackExecuted
                  , assertCallbackType SUT.OnCompletion
                  , assertErrorType "TimeoutError"
                  ]
                , andP
                  [ assertEventType ProcessFailed
                  , assertErrorType "ProcessCallbackFailed"
                  ]
                ]
                []
                Nothing
          ]
        , testGroup
          "workerOnFailure"
          [ testCase "does execute callback when sub-routine fails"
            $ testCapatazStream
                []
                ( \capataz -> do
                  _workerId <- SUT.forkWorker
                    ( SUT.defWorkerOptions
                      { SUT.workerRestartStrategy = SUT.Temporary
                      }
                    )
                    (throwIO RestartingWorkerError)
                    capataz
                  return ()
                )
                [ andP
                  [ assertEventType ProcessCallbackExecuted
                  , assertCallbackType SUT.OnFailure
                  ]
                , assertEventType ProcessFailed
                ]
                [assertEventType CapatazTerminated]
                Nothing
          , testCase "does not execute callback when sub-routine is completed"
            $ testCapatazStream
                []
                ( \capataz -> do
                  _workerId <- SUT.forkWorker
                    ( SUT.defWorkerOptions
                      { SUT.workerRestartStrategy = SUT.Temporary
                      }
                    )
                    (return ())
                    capataz
                  return ()
                )
                [ andP
                  [ assertEventType ProcessCallbackExecuted
                  , assertCallbackType SUT.OnCompletion
                  ]
                , assertEventType ProcessCompleted
                ]
                []
                ( Just
                $ not
                . andP
                    [ assertEventType ProcessCallbackExecuted
                    , assertCallbackType SUT.OnFailure
                    ]
                )
          , testCase "does not execute callback when sub-routine is terminated"
            $ testCapatazStream
                []
                ( \capataz -> do
                  workerId <- SUT.forkWorker
                    ( SUT.defWorkerOptions
                      { SUT.workerRestartStrategy = SUT.Temporary
                      }
                    )
                    (forever $ threadDelay 1000100)
                    capataz

                  SUT.terminateProcess "testing onFailure callback"
                                       workerId
                                       capataz
                )
                [assertEventType ProcessTerminated]
                []
                ( Just
                $ not
                . andP
                    [ assertEventType ProcessCallbackExecuted
                    , assertCallbackType SUT.OnFailure
                    ]
                )
          , testCase "treats as sub-routine failed if callback fails"
            $ testCapatazStream
                []
                ( \capataz -> do
                  _workerId <- SUT.forkWorker
                    ( SUT.defWorkerOptions
                      { SUT.workerRestartStrategy = SUT.Temporary
                      , SUT.workerOnFailure       = const $ throwIO TimeoutError
                      }
                    )
                    (throwIO RestartingWorkerError)
                    capataz

                  return ()
                )
                [ andP
                  [ assertEventType ProcessCallbackExecuted
                  , assertCallbackType SUT.OnFailure
                  , assertErrorType "TimeoutError"
                  ]
                , andP
                  [ assertEventType ProcessFailed
                  , assertErrorType "ProcessCallbackFailed"
                  ]
                ]
                []
                Nothing
          ]
        , testGroup
          "workerOnTermination"
          [ testCase
              "gets brutally killed when TimeoutSeconds termination policy is not met"
            $ testCapatazStream
                []
                ( \capataz -> do
                  workerId <- SUT.forkWorker
                    ( SUT.defWorkerOptions
                      { SUT.workerRestartStrategy = SUT.Temporary
                      , SUT.workerTerminationPolicy = SUT.TimeoutMillis 1
                      , SUT.workerOnTermination = forever $ threadDelay 100100
                      }
                    )
                    (forever $ threadDelay 10001000)
                    capataz

                  SUT.terminateProcess "testing workerOnTermination callback"
                                       workerId
                                       capataz
                )
                [ andP
                  [ assertEventType ProcessCallbackExecuted
                  , assertCallbackType SUT.OnTermination
                  , assertErrorType "BrutallyTerminateProcessException"
                  ]
                , andP
                  [ assertEventType ProcessFailed
                  , assertCallbackType SUT.OnTermination
                  , assertErrorType "ProcessCallbackFailed"
                  ]
                ]
                []
                Nothing
          , testCase "does execute callback when sub-routine is terminated"
            $ testCapatazStream
                []
                ( \capataz -> do
                  workerId <- SUT.forkWorker
                    ( SUT.defWorkerOptions
                      { SUT.workerRestartStrategy = SUT.Temporary
                      }
                    )
                    (forever $ threadDelay 1000100)
                    capataz

                  SUT.terminateProcess "testing workerOnTermination callback"
                                       workerId
                                       capataz
                )
                [ andP
                  [ assertEventType ProcessCallbackExecuted
                  , assertCallbackType SUT.OnTermination
                  ]
                , assertEventType ProcessTerminated
                ]
                [assertEventType CapatazTerminated]
                Nothing
          , testCase "does not execute callback when sub-routine is completed"
            $ testCapatazStream
                []
                ( \capataz -> do
                  _workerId <- SUT.forkWorker
                    ( SUT.defWorkerOptions
                      { SUT.workerRestartStrategy = SUT.Temporary
                      }
                    )
                    (return ())
                    capataz
                  return ()
                )
                [assertEventType ProcessCompleted]
                []
                ( Just $ not . andP
                  [ assertEventType ProcessCallbackExecuted
                  , assertCallbackType SUT.OnTermination
                  ]
                )
          , testCase "does not execute callback when sub-routine fails"
            $ testCapatazStream
                []
                ( \capataz -> do
                  _workerId <- SUT.forkWorker
                    ( SUT.defWorkerOptions
                      { SUT.workerRestartStrategy = SUT.Temporary
                      }
                    )
                    (throwIO (ErrorCall "surprise!"))
                    capataz
                  return ()
                )
                [assertEventType ProcessFailed]
                []
                ( Just $ not . andP
                  [ assertEventType ProcessCallbackExecuted
                  , assertCallbackType SUT.OnTermination
                  ]
                )
          , testCase "treats as sub-routine failed if callback fails"
            $ testCapatazStream
                []
                ( \capataz -> do
                  workerId <- SUT.forkWorker
                    ( SUT.defWorkerOptions
                      { SUT.workerRestartStrategy = SUT.Temporary
                      , SUT.workerOnTermination   = throwIO TimeoutError
                      }
                    )
                    (forever $ threadDelay 10001000)
                    capataz

                  SUT.terminateProcess "testing workerOnTermination callback"
                                       workerId
                                       capataz
                )
                [ andP
                  [ assertEventType ProcessCallbackExecuted
                  , assertCallbackType SUT.OnTermination
                  , assertErrorType "TimeoutError"
                  ]
                , andP
                  [ assertEventType ProcessFailed
                  , assertErrorType "ProcessCallbackFailed"
                  ]
                ]
                []
                Nothing
          ]
        ]
      , testGroup
        "with transient strategy"
        [ testCase "does not restart on completion" $ testCapatazStream
          []
          ( \capataz -> do
            _workerId <- SUT.forkWorker
              SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Transient }
              (return ())
              capataz
            return ()
          )
          [assertEventType ProcessStarted, assertEventType ProcessCompleted]
          [assertEventType CapatazTerminated]
          (Just $ not . assertEventType ProcessRestarted)
        , testCase "does not restart on termination" $ testCapatazStream
          []
          ( \capataz -> do
            workerId <- SUT.forkWorker
              SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Transient }
              (forever $ threadDelay 1000100)
              capataz
            SUT.terminateProcess "termination test (1)" workerId capataz
          )
          [assertEventType ProcessTerminated]
          [assertEventType CapatazTerminated]
          (Just $ not . assertEventType ProcessRestarted)
        , testCase "does restart on failure" $ testCapatazStream
          []
          ( \capataz -> do
            subRoutineAction <- mkFailingSubRoutine 1
            _workerId        <- SUT.forkWorker
              SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Transient }
              subRoutineAction
              capataz
            return ()
          )
          [ assertEventType ProcessStarted
          , assertEventType ProcessFailed
          , andP [assertEventType ProcessRestarted, assertRestartCount (== 1)]
          ]
          []
          Nothing
        , testCase "does increase restart count on multiple failures"
          $ testCapatazStream
              []
              ( \capataz -> do
                subRoutineAction <- mkFailingSubRoutine 2
                _workerId        <- SUT.forkWorker
                  SUT.defWorkerOptions
                    { SUT.workerRestartStrategy = SUT.Transient
                    }
                  subRoutineAction
                  capataz
                return ()
              )
              [ andP
                [assertEventType ProcessRestarted, assertRestartCount (== 1)]
              , andP
                [assertEventType ProcessRestarted, assertRestartCount (== 2)]
              ]
              []
              Nothing
        ]
      , testGroup
        "with permanent strategy"
        [ testCase "does restart on completion" $ testCapatazStream
          []
          ( \capataz -> do
            subRoutineAction <- mkCompletingOnceSubRoutine
            _workerId        <- SUT.forkWorker
              SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Permanent }
              subRoutineAction
              capataz
            return ()
          )
          [ assertEventType ProcessStarted
          , assertEventType ProcessCompleted
          , assertEventType ProcessRestarted
          ]
          [assertEventType CapatazTerminated]
          Nothing
        , testCase "does not increase restart count on multiple completions"
          $ testCapatazStream
              []
              ( \capataz -> do
            -- Note the number is two (2) given the assertion list has two `ProcessRestarted` assertions
                let expectedRestartCount = 2
                subRoutineAction <- mkCompletingBeforeNRestartsSubRoutine
                  expectedRestartCount
                _workerId <- SUT.forkWorker
                  SUT.defWorkerOptions
                    { SUT.workerRestartStrategy = SUT.Permanent
                    }
                  subRoutineAction
                  capataz
                return ()
              )
              [ andP
                [assertEventType ProcessRestarted, assertRestartCount (== 1)]
              , andP
                [assertEventType ProcessRestarted, assertRestartCount (== 1)]
              ]
              []
              Nothing
        , testCase "does restart on termination" $ testCapatazStream
          []
          ( \capataz -> do
            workerId <- SUT.forkWorker
              SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Permanent }
              (forever $ threadDelay 10001000)
              capataz
            SUT.terminateProcess "testing termination (1)" workerId capataz
          )
          [assertEventType ProcessTerminated, assertEventType ProcessRestarted]
          []
          Nothing
        , testCase "does increase restart count on multiple terminations" $ do
          terminationCountVar <- newTVarIO (0 :: Int)
          let signalWorkerTermination =
                atomically (modifyTVar' terminationCountVar (+ 1))
              waitWorkerTermination i = atomically $ do
                n <- readTVar terminationCountVar
                when (n /= i) retry
          testCapatazStream
            []
            ( \capataz -> do
              workerId <- SUT.forkWorker
                SUT.defWorkerOptions
                  { SUT.workerRestartStrategy = SUT.Permanent
                  , SUT.workerOnTermination   = signalWorkerTermination
                  }
                (forever $ threadDelay 10001000)
                capataz

              SUT.terminateProcess "testing termination (1)" workerId capataz
              waitWorkerTermination 1
              SUT.terminateProcess "testing termination (2)" workerId capataz
              waitWorkerTermination 2
            )
            [ assertEventType ProcessTerminated
            , andP [assertEventType ProcessRestarted, assertRestartCount (== 1)]
            , assertEventType ProcessTerminated
            , andP [assertEventType ProcessRestarted, assertRestartCount (== 2)]
            ]
            []
            Nothing
        , testCase "does restart on failure" $ testCapatazStream
          []
          ( \capataz -> do
            subRoutineAction <- mkFailingSubRoutine 1
            _workerId        <- SUT.forkWorker
              SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Permanent }
              subRoutineAction
              capataz
            return ()
          )
          [ assertEventType ProcessStarted
          , assertEventType ProcessFailed
          , andP [assertEventType ProcessRestarted, assertRestartCount (== 1)]
          ]
          []
          Nothing
        , testCase "does increase restart count on multiple failures"
          $ testCapatazStream
              []
              ( \capataz -> do
                subRoutineAction <- mkFailingSubRoutine 2
                _workerId        <- SUT.forkWorker
                  SUT.defWorkerOptions
                    { SUT.workerRestartStrategy = SUT.Permanent
                    }
                  subRoutineAction
                  capataz
                return ()
              )
              [ andP
                [assertEventType ProcessRestarted, assertRestartCount (== 1)]
              , andP
                [assertEventType ProcessRestarted, assertRestartCount (== 2)]
              ]
              []
              Nothing
        ]
      , testGroup
        "with temporary strategy"
        [ testCase "does not restart on completion" $ testCapatazStream
          []
          ( \capataz -> do
            _workerId <- SUT.forkWorker
              SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Temporary }
              (return ())
              capataz
            return ()
          )
          [assertEventType ProcessStarted, assertEventType ProcessCompleted]
          [assertEventType CapatazTerminated]
          (Just $ not . assertEventType ProcessRestarted)
        , testCase "does not restart on termination" $ testCapatazStream
          []
          ( \capataz -> do
            workerId <- SUT.forkWorker
              SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Temporary }
              (forever $ threadDelay 1000100)
              capataz
            SUT.terminateProcess "termination test (1)" workerId capataz
            threadDelay 100
          )
          [assertEventType ProcessStarted, assertEventType ProcessTerminated]
          [assertEventType CapatazTerminated]
          (Just $ not . assertEventType ProcessRestarted)
        , testCase "does not restart on failure" $ testCapatazStream
          []
          ( \capataz -> do
            _workerId <- SUT.forkWorker
              SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Temporary }
              (panic "worker failed!")
              capataz
            threadDelay 100
          )
          [assertEventType ProcessStarted, assertEventType ProcessFailed]
          [assertEventType CapatazTerminated]
          (Just $ not . assertEventType ProcessRestarted)
        ]
      ]
    , testGroup
      "multiple supervised IO sub-routines"
      [ testCase "terminates all supervised sub-routines on teardown"
        $ testCapatazStream
            []
            ( \capataz -> do
              _workerA <- SUT.forkWorker
                SUT.defWorkerOptions { SUT.workerName            = "A"
                                     , SUT.workerRestartStrategy = SUT.Permanent
                                     }
                (forever $ threadDelay 1000100)
                capataz


              _workerB <- SUT.forkWorker
                SUT.defWorkerOptions { SUT.workerName            = "B"
                                     , SUT.workerRestartStrategy = SUT.Permanent
                                     }
                (forever $ threadDelay 1000100)
                capataz

              return ()
            )
            [ andP [assertEventType ProcessStarted, assertWorkerName "A"]
            , andP [assertEventType ProcessStarted, assertWorkerName "B"]
            ]
            [ andP [assertEventType ProcessTerminated, assertWorkerName "A"]
            , andP [assertEventType ProcessTerminated, assertWorkerName "B"]
            , assertEventType CapatazTerminated
            ]
            Nothing
      , testGroup
        "with one for one capataz restart strategy"
        [ testCase "restarts failing sub-routine only"
            $ testCapatazStreamWithOptions
                []
                ( \supOptions ->
                  supOptions { SUT.supervisorRestartStrategy = SUT.OneForOne }
                )
                ( \capataz -> do
                  _workerA <- SUT.forkWorker
                    SUT.defWorkerOptions
                      { SUT.workerName            = "A"
                      , SUT.workerRestartStrategy = SUT.Temporary
                      }
                    (forever $ threadDelay 1000100)
                    capataz

                  ioB      <- mkFailingSubRoutine 1

                  _workerB <- SUT.forkWorker
                    SUT.defWorkerOptions
                      { SUT.workerName            = "B"
                      , SUT.workerRestartStrategy = SUT.Permanent
                      }
                    (forever $ ioB >> threadDelay 1000100)
                    capataz

                  return ()
                )
                [andP [assertEventType ProcessRestarted, assertWorkerName "B"]]
                []
                ( Just $ not . andP
                  [assertEventType ProcessRestarted, assertWorkerName "A"]
                )
        ]
      , testGroup
        "with all for one capataz restart strategy with newest first order"
        [ testCase "does terminate all other workers that did not fail"
          $ testCapatazStreamWithOptions
              []
              ( \supOptions -> supOptions
                { SUT.supervisorRestartStrategy         = SUT.AllForOne
                , SUT.supervisorProcessTerminationOrder = SUT.OldestFirst
                }
              )
              ( \capataz -> do
              -- This lockVar guarantees that workerB executes before workerA
                lockVar  <- newEmptyMVar

                ioA      <- mkFailingSubRoutine 1

                _workerA <- SUT.forkWorker
                  SUT.defWorkerOptions
                    { SUT.workerName            = "A"
                    , SUT.workerRestartStrategy = SUT.Permanent
                    }
                  (forever $ readMVar lockVar >> ioA)
                  capataz

                _workerB <- SUT.forkWorker
                  SUT.defWorkerOptions
                    { SUT.workerName            = "B"
                    , SUT.workerRestartStrategy = SUT.Permanent
                    }
                  (putMVar lockVar () >> forever (threadDelay 10))
                  capataz

                return ()
              )
              [ andP [assertEventType ProcessStarted, assertWorkerName "A"]
              , andP [assertEventType ProcessStarted, assertWorkerName "B"]
              , andP [assertEventType ProcessFailed, assertWorkerName "A"]
              , andP [assertEventType ProcessRestarted, assertWorkerName "A"]
              , andP [assertEventType ProcessTerminated, assertWorkerName "B"]
              , andP [assertEventType ProcessRestarted, assertWorkerName "B"]
              ]
              []
              Nothing
        , testCase "does not restart sub-routines that are temporary"
          $ testCapatazStreamWithOptions
              []
              ( \supOptions -> supOptions
                { SUT.supervisorRestartStrategy         = SUT.AllForOne
                , SUT.supervisorProcessTerminationOrder = SUT.OldestFirst
                }
              )
              ( \capataz -> do
                lockVar  <- newEmptyMVar

                ioA      <- mkFailingSubRoutine 1

                _workerA <- SUT.forkWorker
                  SUT.defWorkerOptions
                    { SUT.workerName            = "A"
                    , SUT.workerRestartStrategy = SUT.Permanent
                    }
                  (forever $ readMVar lockVar >> ioA)
                  capataz

                _workerB <- SUT.forkWorker
                  SUT.defWorkerOptions
                    { SUT.workerName            = "B"
                    , SUT.workerRestartStrategy = SUT.Temporary
                    }
                  (putMVar lockVar () >> forever (threadDelay 10))
                  capataz

                return ()
              )
              [ andP [assertEventType ProcessStarted, assertWorkerName "A"]
              , andP [assertEventType ProcessStarted, assertWorkerName "B"]
              , andP [assertEventType ProcessFailed, assertWorkerName "A"]
              , andP [assertEventType ProcessRestarted, assertWorkerName "A"]
              , andP [assertEventType ProcessTerminated, assertWorkerName "B"]
              ]
              []
              ( Just $ not . andP
                [assertEventType ProcessRestarted, assertWorkerName "B"]
              )
        , testCase "restarts sub-routines that are not temporary"
          $ testCapatazStreamWithOptions
              []
              ( \supOptions -> supOptions
                { SUT.supervisorRestartStrategy         = SUT.AllForOne
                , SUT.supervisorProcessTerminationOrder = SUT.NewestFirst
                }
              )
              ( \capataz -> do
                ioA      <- mkFailingSubRoutine 1

                lockVar  <- newEmptyMVar

                _workerA <- SUT.forkWorker
                  SUT.defWorkerOptions
                    { SUT.workerName            = "A"
                    , SUT.workerRestartStrategy = SUT.Permanent
                    }
                  (forever $ readMVar lockVar >> ioA)
                  capataz

                _workerB <- SUT.forkWorker
                  SUT.defWorkerOptions
                    { SUT.workerName            = "B"
                    , SUT.workerRestartStrategy = SUT.Transient
                    }
                  (putMVar lockVar () >> forever (threadDelay 10))
                  capataz

                return ()
              )
              [ andP [assertEventType ProcessRestarted, assertWorkerName "B"]
              , andP [assertEventType ProcessRestarted, assertWorkerName "A"]
              ]
              []
              Nothing
        ]
      , testGroup
        "with all for one capataz restart strategy with oldest first order"
        [ testCase "does not restart sub-routines that are temporary"
          $ testCapatazStreamWithOptions
              []
              ( \supOptions -> supOptions
                { SUT.supervisorRestartStrategy         = SUT.AllForOne
                , SUT.supervisorProcessTerminationOrder = SUT.OldestFirst
                }
              )
              ( \capataz -> do
                ioA      <- mkFailingSubRoutine 1

                -- This lockVar guarantees that workerB executes before workerA
                lockVar  <- newEmptyMVar

                _workerA <- SUT.forkWorker
                  SUT.defWorkerOptions
                    { SUT.workerName            = "A"
                    , SUT.workerRestartStrategy = SUT.Permanent
                    }
                  (forever $ readMVar lockVar >> ioA)
                  capataz

                _workerB <- SUT.forkWorker
                  SUT.defWorkerOptions
                    { SUT.workerName            = "B"
                    , SUT.workerRestartStrategy = SUT.Temporary
                    }
                  (putMVar lockVar () >> forever (threadDelay 10))
                  capataz

                return ()
              )
              [andP [assertEventType ProcessRestarted, assertWorkerName "A"]]
              []
              ( Just $ not . andP
                [assertEventType ProcessRestarted, assertWorkerName "B"]
              )
        , testCase "restarts sub-routines that are not temporary"
          $ testCapatazStreamWithOptions
              []
              ( \supOptions -> supOptions
                { SUT.supervisorRestartStrategy         = SUT.AllForOne
                , SUT.supervisorProcessTerminationOrder = SUT.OldestFirst
                }
              )
              ( \capataz -> do
                ioA      <- mkFailingSubRoutine 1

                -- This lockVar guarantees that workerB executes before workerA
                lockVar  <- newEmptyMVar

                _workerA <- SUT.forkWorker
                  SUT.defWorkerOptions
                    { SUT.workerName            = "A"
                    , SUT.workerRestartStrategy = SUT.Permanent
                    }
                  (forever $ readMVar lockVar >> ioA)
                  capataz

                _workerB <- SUT.forkWorker
                  SUT.defWorkerOptions
                    { SUT.workerName            = "B"
                    , SUT.workerRestartStrategy = SUT.Transient
                    }
                  (putMVar lockVar () >> forever (threadDelay 10))
                  capataz

                return ()
              )
              [ andP [assertEventType ProcessRestarted, assertWorkerName "A"]
              , andP [assertEventType ProcessRestarted, assertWorkerName "B"]
              ]
              []
              Nothing
        ]
      ]
    ]

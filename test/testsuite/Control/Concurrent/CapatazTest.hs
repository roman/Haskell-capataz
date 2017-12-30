{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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

fetchRecordName :: Show a => a -> Text
fetchRecordName = T.takeWhile (/= ' ') . show

andP :: [a -> Bool] -> a -> Bool
andP predList a = all ($ a) predList

-- orP :: [a -> Bool] -> a -> Bool
-- orP predList a = any ($ a) predList

--------------------------------------------------------------------------------
-- Assertions and Testers

data EventType
  = InvalidCapatazStatusReached
  | CapatazStatusChanged
  | WorkerTerminated
  | WorkerStarted
  | WorkerRestarted
  | WorkerCompleted
  | WorkerFailed
  | WorkerCallbackExecuted
  | WorkersTerminationStarted
  | WorkersTerminationFinished
  | CapatazFailed
  | CapatazTerminated
  deriving (Show)

assertEventType :: EventType -> SUT.CapatazEvent -> Bool
assertEventType evType ev = fetchRecordName ev == show evType

assertWorkerName :: Text -> SUT.CapatazEvent -> Bool
assertWorkerName workerName' ev = case ev of
  SUT.WorkerRestarted { workerName }  -> workerName' == workerName
  SUT.WorkerFailed { workerName }     -> workerName' == workerName
  SUT.WorkerTerminated { workerName } -> workerName' == workerName
  SUT.WorkerStarted { workerName }    -> workerName' == workerName
  _                                           -> False

assertErrorType :: Text -> SUT.CapatazEvent -> Bool
assertErrorType errType ev = case ev of
  SUT.WorkerFailed { workerError } ->
    fetchRecordName workerError == errType
  SUT.CapatazFailed { capatazError } ->
    fetchRecordName capatazError == errType
  SUT.WorkerCallbackExecuted { workerCallbackError } ->
    case workerCallbackError of
      Nothing            -> False
      Just originalError -> fetchRecordName originalError == errType
  _ -> False

assertCallbackType :: SUT.CallbackType -> SUT.CapatazEvent -> Bool
assertCallbackType cbType ev = case ev of
  SUT.WorkerFailed { workerError } -> case fromException workerError of
    Just SUT.WorkerCallbackFailed { callbackType } -> cbType == callbackType
    _                                             -> False
  SUT.WorkerCallbackExecuted { callbackType } ->
    cbType == callbackType
  _ -> False

assertRestartCount :: (Int -> Bool) -> SUT.CapatazEvent -> Bool
assertRestartCount predFn ev = case ev of
  SUT.WorkerRestarted { workerRestartCount } ->
    predFn workerRestartCount
  _ -> False

assertCapatazStatusChanged
  :: SUT.CapatazStatus -> SUT.CapatazStatus -> SUT.CapatazEvent -> Bool
assertCapatazStatusChanged fromEv toEv ev = case ev of
  SUT.CapatazStatusChanged { prevCapatazStatus, newCapatazStatus } ->
    fromEv == prevCapatazStatus && toEv == newCapatazStatus
  _ -> False

assertWorkerStarted :: Text -> SUT.CapatazEvent -> Bool
assertWorkerStarted workerName =
  andP [assertEventType WorkerStarted, assertWorkerName workerName]

assertWorkerTerminated :: Text -> SUT.CapatazEvent -> Bool
assertWorkerTerminated workerName =
  andP [assertEventType WorkerTerminated, assertWorkerName workerName]

assertCapatazFailedWith :: Text -> SUT.CapatazEvent -> Bool
assertCapatazFailedWith errorName =
  andP [assertEventType CapatazFailed, assertErrorType errorName]

--------------------------------------------------------------------------------

data RestartingWorkerError
  = RestartingWorkerError
  deriving (Show)

instance Exception RestartingWorkerError

data TimeoutError
  = TimeoutError
  deriving (Show)

instance Exception TimeoutError

mkFailingSubRoutine :: Int -> IO (IO ())
mkFailingSubRoutine failCount = do
  countRef <- newIORef failCount
  let subRoutine = do
        shouldFail <- atomicModifyIORef' countRef
                                         (\count -> (pred count, count > 0))
        when shouldFail (throwIO RestartingWorkerError)

  return subRoutine

-- | Returns two values:
--
-- * A sub-routine that will complete for `initCount` amount of
--   times
-- * A sub-routine that will release a lock every time it is restarted
--
-- This function works great when testing `Permanent` strategies, as you would
-- like to assert restart events once (if it keeps completing it will fill up the
-- log with restart events)
--
mkCompletingBeforeNRestartsSubRoutine :: Int -> IO (IO ())
mkCompletingBeforeNRestartsSubRoutine initCount = do
  countRef <- newIORef initCount
  let subRoutine = do
        shouldStop <- atomicModifyIORef' countRef
                                         (\count -> (pred count, count > 0))
        if shouldStop then return () else forever $ threadDelay 1000100
  return subRoutine

-- | Returns two values:
--
-- * A sub-routine that will complete once
--
-- This function works great when testing `Permanent` strategies, as you would
-- like to assert restart events once (if it keeps completing it will fill up the
-- log with restart events)
--
mkCompletingOnceSubRoutine :: IO (IO ())
mkCompletingOnceSubRoutine = mkCompletingBeforeNRestartsSubRoutine 1

testCapatazStreamWithOptions
  :: [SUT.CapatazEvent -> Bool]
  -> (SUT.CapatazOptions -> SUT.CapatazOptions)
  -> (SUT.Capataz -> IO ())
  -> [SUT.CapatazEvent -> Bool]
  -> [SUT.CapatazEvent -> Bool]
  -> Maybe (SUT.CapatazEvent -> Bool)
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

    runAssertions (eventStream, accRef)
                  pendingCountVar
                  preSetupAssertion
                  capataz
    setupResult <- try (setupFn capataz)

    case setupResult of
      Left  err -> assertFailure (show (err :: SomeException))
      Right _   -> do
        runAssertions (eventStream, accRef)
                      pendingCountVar
                      postSetupAssertions
                      capataz

        void $ SUT.teardown capataz
        runAssertions (eventStream, accRef)
                      pendingCountVar
                      postTeardownAssertions
                      capataz

        case mAllEventsAssertion of
          Nothing                 -> return ()
          Just allEventsAssertion -> do
            events <- reverse <$> readIORef accRef
            assertBool
              (  "Expected all events to match predicate, but didn't ("
              <> show (length events)
              <> " events tried)\n"
              <> ppShow (zip ([0 ..] :: [Int]) events)
              )
              (all allEventsAssertion events)
 where
  runAssertions (eventStream, accRef) pendingCountVar assertions capataz =
    do
      raceResult <- race
        (threadDelay 1000100)
        (readEventLoop eventStream pendingCountVar assertions)
      case raceResult of
        Left _ -> do
          events       <- reverse <$> readIORef accRef
          pendingCount <- readIORef pendingCountVar
          void $ SUT.teardown capataz
          assertFailure
            (  "Expected all assertions to match, but didn't ("
            <> show pendingCount
            <> " assertions remaining, "
            <> show (length events)
            <> " events tried)\n"
            <> ppShow (zip ([0 ..] :: [Int]) events)
            )
        Right _ -> return ()


  trackEvent accRef eventStream event = do
    atomicModifyIORef' accRef (\old -> (event : old, ()))
    atomically $ writeTQueue eventStream event

  readEventLoop eventStream pendingCount assertions = do
    writeIORef pendingCount (length assertions)
    case assertions of
      []                        -> return ()
      (assertionFn:assertions1) -> do
        event <- atomically $ readTQueue eventStream
        if assertionFn event
          then readEventLoop eventStream pendingCount assertions1
          else readEventLoop eventStream pendingCount assertions


testCapatazStream
  :: [SUT.CapatazEvent -> Bool]
  -> (SUT.Capataz -> IO ())
  -> [SUT.CapatazEvent -> Bool]
  -> [SUT.CapatazEvent -> Bool]
  -> Maybe (SUT.CapatazEvent -> Bool)
  -> IO ()
testCapatazStream preSetupAssertions =
  testCapatazStreamWithOptions preSetupAssertions identity

--------------------------------------------------------------------------------
-- Actual Tests

tests :: [TestTree]
tests
  = [ testGroup
      "capataz without workerSpecList"
      [ testCase "initialize and teardown works as expected"
          $ testCapatazStream
              [ andP
                  [ assertEventType CapatazStatusChanged
                  , assertCapatazStatusChanged SUT.Initializing SUT.Running
                  ]
              ]
              (const $ return ())
              []
              [ andP
                [ assertEventType CapatazStatusChanged
                , assertCapatazStatusChanged SUT.Running SUT.Halting
                ]
              , andP
                [ assertEventType CapatazStatusChanged
                , assertCapatazStatusChanged SUT.Halting SUT.Halted
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
                [ assertEventType CapatazStatusChanged
                , assertCapatazStatusChanged SUT.Initializing SUT.Running
                ]
              ]
              ( \supOptions -> supOptions
                { SUT.capatazWorkerSpecList = [ SUT.defWorkerSpec
                                                  { SUT.workerName   = "A"
                                                  , SUT.workerAction = forever
                                                    (threadDelay 10001000)
                                                  }
                                                , SUT.defWorkerSpec
                                                  { SUT.workerName   = "B"
                                                  , SUT.workerAction = forever
                                                    (threadDelay 10001000)
                                                  }
                                                ]
                }
              )
              (const $ return ())
              []
              [ andP
                [ assertEventType CapatazStatusChanged
                , assertCapatazStatusChanged SUT.Running SUT.Halting
                ]
              , assertEventType WorkersTerminationStarted
              , assertWorkerTerminated "A"
              , assertWorkerTerminated "B"
              , assertEventType WorkersTerminationFinished
              , andP
                [ assertEventType CapatazStatusChanged
                , assertCapatazStatusChanged SUT.Halting SUT.Halted
                ]
              ]
              Nothing
      ]
    , testCase
        "reports error when capataz thread receives async exception"
      $ testCapatazStream
          [ andP
              [ assertEventType CapatazStatusChanged
              , assertCapatazStatusChanged SUT.Initializing SUT.Running
              ]
          ]
          ( \SUT.Capataz { capatazAsync } -> do
            threadDelay 100 -- leave enough room for capataz to start
            cancelWith capatazAsync (ErrorCall "async exception")
          )
          [assertEventType CapatazFailed]
          []
          Nothing
    , testCase
        "reports error when worker retries violate restart intensity"
      $ do
          lockVar <- newEmptyMVar
          let (signalIntensityReached, waitTillIntensityReached) =
                (putMVar lockVar (), takeMVar lockVar)
          testCapatazStreamWithOptions
            []
            ( \supOptions -> supOptions
              { SUT.onCapatazIntensityReached = signalIntensityReached
              }
            )
            ( \capataz -> do
              _workerId <- SUT.forkWorker SUT.defWorkerOptions
                                        (throwIO RestartingWorkerError)
                                        capataz
              waitTillIntensityReached
            )
            [ assertEventType WorkerFailed
            , assertEventType WorkerFailed
            , assertEventType WorkerFailed
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
                  [ assertEventType WorkerCallbackExecuted
                  , assertCallbackType SUT.OnCompletion
                  ]
                , assertEventType WorkerCompleted
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
                  [ assertEventType WorkerCallbackExecuted
                  , assertCallbackType SUT.OnFailure
                  ]
                , assertEventType WorkerFailed
                ]
                [assertEventType CapatazTerminated]
                ( Just $ not . andP
                  [ assertEventType WorkerCallbackExecuted
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

                  _workerId <- SUT.terminateWorker
                    "testing onCompletion callback"
                    workerId
                    capataz
                  return ()
                )
                [assertEventType WorkerTerminated]
                [assertEventType CapatazTerminated]
                ( Just $ not . andP
                  [ assertEventType WorkerCallbackExecuted
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
                  [ assertEventType WorkerCallbackExecuted
                  , assertCallbackType SUT.OnCompletion
                  , assertErrorType "TimeoutError"
                  ]
                , andP
                  [ assertEventType WorkerFailed
                  , assertErrorType "WorkerCallbackFailed"
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
                  [ assertEventType WorkerCallbackExecuted
                  , assertCallbackType SUT.OnFailure
                  ]
                , assertEventType WorkerFailed
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
                  [ assertEventType WorkerCallbackExecuted
                  , assertCallbackType SUT.OnCompletion
                  ]
                , assertEventType WorkerCompleted
                ]
                []
                ( Just $ not . andP
                  [ assertEventType WorkerCallbackExecuted
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

                  SUT.terminateWorker "testing onFailure callback"
                                     workerId
                                     capataz
                )
                [assertEventType WorkerTerminated]
                []
                ( Just $ not . andP
                  [ assertEventType WorkerCallbackExecuted
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
                  [ assertEventType WorkerCallbackExecuted
                  , assertCallbackType SUT.OnFailure
                  , assertErrorType "TimeoutError"
                  ]
                , andP
                  [ assertEventType WorkerFailed
                  , assertErrorType "WorkerCallbackFailed"
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

                  SUT.terminateWorker "testing workerOnTermination callback"
                                     workerId
                                     capataz
                )
                [ andP
                  [ assertEventType WorkerCallbackExecuted
                  , assertCallbackType SUT.OnTermination
                  , assertErrorType "BrutallyTerminateWorkerException"
                  ]
                , andP
                  [ assertEventType WorkerFailed
                  , assertErrorType "WorkerCallbackFailed"
                  , assertCallbackType SUT.OnTermination
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

                  SUT.terminateWorker "testing workerOnTermination callback"
                                     workerId
                                     capataz
                )
                [ andP
                  [ assertEventType WorkerCallbackExecuted
                  , assertCallbackType SUT.OnTermination
                  ]
                , assertEventType WorkerTerminated
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
                [assertEventType WorkerCompleted]
                []
                ( Just $ not . andP
                  [ assertEventType WorkerCallbackExecuted
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
                [assertEventType WorkerFailed]
                []
                ( Just $ not . andP
                  [ assertEventType WorkerCallbackExecuted
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

                  SUT.terminateWorker "testing workerOnTermination callback"
                                     workerId
                                     capataz
                )
                [ andP
                  [ assertEventType WorkerCallbackExecuted
                  , assertCallbackType SUT.OnTermination
                  , assertErrorType "TimeoutError"
                  ]
                , andP
                  [ assertEventType WorkerFailed
                  , assertErrorType "WorkerCallbackFailed"
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
          [ assertEventType WorkerStarted
          , assertEventType WorkerCompleted
          ]
          [assertEventType CapatazTerminated]
          (Just $ not . assertEventType WorkerRestarted)
        , testCase "does not restart on termination" $ testCapatazStream
          []
          ( \capataz -> do
            workerId <- SUT.forkWorker
              SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Transient }
              (forever $ threadDelay 1000100)
              capataz
            SUT.terminateWorker "termination test (1)" workerId capataz
          )
          [assertEventType WorkerTerminated]
          [assertEventType CapatazTerminated]
          (Just $ not . assertEventType WorkerRestarted)
        , testCase "does restart on failure" $ testCapatazStream
          []
          ( \capataz -> do
            subRoutineAction <- mkFailingSubRoutine 1
            _workerId         <- SUT.forkWorker
              SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Transient }
              subRoutineAction
              capataz
            return ()
          )
          [ assertEventType WorkerStarted
          , assertEventType WorkerFailed
          , andP
            [ assertEventType WorkerRestarted
            , assertRestartCount (== 1)
            ]
          ]
          []
          Nothing
        , testCase "does increase restart count on multiple failures"
          $ testCapatazStream
              []
              ( \capataz -> do
                subRoutineAction <- mkFailingSubRoutine 2
                _workerId         <- SUT.forkWorker
                  SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Transient
                                      }
                  subRoutineAction
                  capataz
                return ()
              )
              [ andP
                [ assertEventType WorkerRestarted
                , assertRestartCount (== 1)
                ]
              , andP
                [ assertEventType WorkerRestarted
                , assertRestartCount (== 2)
                ]
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
            _workerId         <- SUT.forkWorker
              SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Permanent }
              subRoutineAction
              capataz
            return ()
          )
          [ assertEventType WorkerStarted
          , assertEventType WorkerCompleted
          , assertEventType WorkerRestarted
          ]
          [assertEventType CapatazTerminated]
          Nothing
        , testCase "does not increase restart count on multiple completions"
          $ testCapatazStream
              []
              ( \capataz -> do
            -- Note the number is two (2) given the assertion list has two `WorkerRestarted` assertions
                let expectedRestartCount = 2
                subRoutineAction <- mkCompletingBeforeNRestartsSubRoutine
                  expectedRestartCount
                _workerId <- SUT.forkWorker
                  SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Permanent
                                      }
                  subRoutineAction
                  capataz
                return ()
              )
              [ andP
                [ assertEventType WorkerRestarted
                , assertRestartCount (== 1)
                ]
              , andP
                [ assertEventType WorkerRestarted
                , assertRestartCount (== 1)
                ]
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
            SUT.terminateWorker "testing termination (1)" workerId capataz
          )
          [ assertEventType WorkerTerminated
          , assertEventType WorkerRestarted
          ]
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

              SUT.terminateWorker "testing termination (1)" workerId capataz
              waitWorkerTermination 1
              SUT.terminateWorker "testing termination (2)" workerId capataz
              waitWorkerTermination 2
            )
            [ assertEventType WorkerTerminated
            , andP
              [ assertEventType WorkerRestarted
              , assertRestartCount (== 1)
              ]
            , assertEventType WorkerTerminated
            , andP
              [ assertEventType WorkerRestarted
              , assertRestartCount (== 2)
              ]
            ]
            []
            Nothing
        , testCase "does restart on failure" $ testCapatazStream
          []
          ( \capataz -> do
            subRoutineAction <- mkFailingSubRoutine 1
            _workerId         <- SUT.forkWorker
              SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Permanent }
              subRoutineAction
              capataz
            return ()
          )
          [ assertEventType WorkerStarted
          , assertEventType WorkerFailed
          , andP
            [ assertEventType WorkerRestarted
            , assertRestartCount (== 1)
            ]
          ]
          []
          Nothing
        , testCase "does increase restart count on multiple failures"
          $ testCapatazStream
              []
              ( \capataz -> do
                subRoutineAction <- mkFailingSubRoutine 2
                _workerId         <- SUT.forkWorker
                  SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Permanent
                                      }
                  subRoutineAction
                  capataz
                return ()
              )
              [ andP
                [ assertEventType WorkerRestarted
                , assertRestartCount (== 1)
                ]
              , andP
                [ assertEventType WorkerRestarted
                , assertRestartCount (== 2)
                ]
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
          [ assertEventType WorkerStarted
          , assertEventType WorkerCompleted
          ]
          [assertEventType CapatazTerminated]
          (Just $ not . assertEventType WorkerRestarted)
        , testCase "does not restart on termination" $ testCapatazStream
          []
          ( \capataz -> do
            workerId <- SUT.forkWorker
              SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Temporary }
              (forever $ threadDelay 1000100)
              capataz
            SUT.terminateWorker "termination test (1)" workerId capataz
            threadDelay 100
          )
          [ assertEventType WorkerStarted
          , assertEventType WorkerTerminated
          ]
          [assertEventType CapatazTerminated]
          (Just $ not . assertEventType WorkerRestarted)
        , testCase "does not restart on failure" $ testCapatazStream
          []
          ( \capataz -> do
            _workerId <- SUT.forkWorker
              SUT.defWorkerOptions { SUT.workerRestartStrategy = SUT.Temporary }
              (panic "worker failed!")
              capataz
            threadDelay 100
          )
          [ assertEventType WorkerStarted
          , assertEventType WorkerFailed
          ]
          [assertEventType CapatazTerminated]
          (Just $ not . assertEventType WorkerRestarted)
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
            [ andP [assertEventType WorkerStarted, assertWorkerName "A"]
            , andP [assertEventType WorkerStarted, assertWorkerName "B"]
            ]
            [ andP
              [assertEventType WorkerTerminated, assertWorkerName "A"]
            , andP
              [assertEventType WorkerTerminated, assertWorkerName "B"]
            , assertEventType CapatazTerminated
            ]
            Nothing
      , testGroup
        "with one for one capataz restart strategy"
        [ testCase "restarts failing sub-routine only"
            $ testCapatazStreamWithOptions
                []
                ( \supOptions ->
                  supOptions { SUT.capatazRestartStrategy = SUT.OneForOne }
                )
                ( \capataz -> do
                  _workerA <- SUT.forkWorker
                    SUT.defWorkerOptions
                      { SUT.workerName            = "A"
                      , SUT.workerRestartStrategy = SUT.Temporary
                      }
                    (forever $ threadDelay 1000100)
                    capataz

                  ioB     <- mkFailingSubRoutine 1

                  _workerB <- SUT.forkWorker
                    SUT.defWorkerOptions
                      { SUT.workerName            = "B"
                      , SUT.workerRestartStrategy = SUT.Permanent
                      }
                    (forever $ ioB >> threadDelay 1000100)
                    capataz

                  return ()
                )
                [ andP
                    [ assertEventType WorkerRestarted
                    , assertWorkerName "B"
                    ]
                ]
                []
                ( Just
                $ not
                . andP
                    [ assertEventType WorkerRestarted
                    , assertWorkerName "A"
                    ]
                )
        ]
      , testGroup
        "with all for one capataz restart strategy with newest first order"
        [ testCase "does terminate all other workers that did not fail"
          $ testCapatazStreamWithOptions
              []
              ( \supOptions -> supOptions
                { SUT.capatazRestartStrategy       = SUT.AllForOne
                , SUT.capatazWorkerTerminationOrder = SUT.OldestFirst
                }
              )
              ( \capataz -> do
              -- This lockVar guarantees that workerB executes before workerA
                lockVar <- newEmptyMVar

                ioA     <- mkFailingSubRoutine 1

                _workerA <- SUT.forkWorker
                  SUT.defWorkerOptions { SUT.workerName            = "A"
                                      , SUT.workerRestartStrategy = SUT.Permanent
                                      }
                  (forever $ readMVar lockVar >> ioA)
                  capataz

                _workerB <- SUT.forkWorker
                  SUT.defWorkerOptions { SUT.workerName            = "B"
                                      , SUT.workerRestartStrategy = SUT.Permanent
                                      }
                  (putMVar lockVar () >> forever (threadDelay 10))
                  capataz

                return ()
              )
              [ andP
                [assertEventType WorkerStarted, assertWorkerName "A"]
              , andP
                [assertEventType WorkerStarted, assertWorkerName "B"]
              , andP
                [assertEventType WorkerFailed, assertWorkerName "A"]
              , andP
                [assertEventType WorkerRestarted, assertWorkerName "A"]
              , andP
                [assertEventType WorkerTerminated, assertWorkerName "B"]
              , andP
                [assertEventType WorkerRestarted, assertWorkerName "B"]
              ]
              []
              Nothing
        , testCase "does not restart sub-routines that are temporary"
          $ testCapatazStreamWithOptions
              []
              ( \supOptions -> supOptions
                { SUT.capatazRestartStrategy       = SUT.AllForOne
                , SUT.capatazWorkerTerminationOrder = SUT.OldestFirst
                }
              )
              ( \capataz -> do
                lockVar <- newEmptyMVar

                ioA     <- mkFailingSubRoutine 1

                _workerA <- SUT.forkWorker
                  SUT.defWorkerOptions { SUT.workerName            = "A"
                                      , SUT.workerRestartStrategy = SUT.Permanent
                                      }
                  (forever $ readMVar lockVar >> ioA)
                  capataz

                _workerB <- SUT.forkWorker
                  SUT.defWorkerOptions { SUT.workerName            = "B"
                                      , SUT.workerRestartStrategy = SUT.Temporary
                                      }
                  (putMVar lockVar () >> forever (threadDelay 10))
                  capataz

                return ()
              )
              [ andP
                [assertEventType WorkerStarted, assertWorkerName "A"]
              , andP
                [assertEventType WorkerStarted, assertWorkerName "B"]
              , andP
                [assertEventType WorkerFailed, assertWorkerName "A"]
              , andP
                [assertEventType WorkerRestarted, assertWorkerName "A"]
              , andP
                [assertEventType WorkerTerminated, assertWorkerName "B"]
              ]
              []
              ( Just
              $ not
              . andP
                  [ assertEventType WorkerRestarted
                  , assertWorkerName "B"
                  ]
              )
        , testCase "restarts sub-routines that are not temporary"
          $ testCapatazStreamWithOptions
              []
              ( \supOptions -> supOptions
                { SUT.capatazRestartStrategy       = SUT.AllForOne
                , SUT.capatazWorkerTerminationOrder = SUT.NewestFirst
                }
              )
              ( \capataz -> do
                ioA     <- mkFailingSubRoutine 1

                lockVar <- newEmptyMVar

                _workerA <- SUT.forkWorker
                  SUT.defWorkerOptions { SUT.workerName            = "A"
                                      , SUT.workerRestartStrategy = SUT.Permanent
                                      }
                  (forever $ readMVar lockVar >> ioA)
                  capataz

                _workerB <- SUT.forkWorker
                  SUT.defWorkerOptions { SUT.workerName            = "B"
                                      , SUT.workerRestartStrategy = SUT.Transient
                                      }
                  (putMVar lockVar () >> forever (threadDelay 10))
                  capataz

                return ()
              )
              [ andP
                [assertEventType WorkerRestarted, assertWorkerName "B"]
              , andP
                [assertEventType WorkerRestarted, assertWorkerName "A"]
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
                { SUT.capatazRestartStrategy       = SUT.AllForOne
                , SUT.capatazWorkerTerminationOrder = SUT.OldestFirst
                }
              )
              ( \capataz -> do
                ioA     <- mkFailingSubRoutine 1

                -- This lockVar guarantees that workerB executes before workerA
                lockVar <- newEmptyMVar

                _workerA <- SUT.forkWorker
                  SUT.defWorkerOptions { SUT.workerName            = "A"
                                      , SUT.workerRestartStrategy = SUT.Permanent
                                      }
                  (forever $ readMVar lockVar >> ioA)
                  capataz

                _workerB <- SUT.forkWorker
                  SUT.defWorkerOptions { SUT.workerName            = "B"
                                      , SUT.workerRestartStrategy = SUT.Temporary
                                      }
                  (putMVar lockVar () >> forever (threadDelay 10))
                  capataz

                return ()
              )
              [ andP
                  [ assertEventType WorkerRestarted
                  , assertWorkerName "A"
                  ]
              ]
              []
              ( Just
              $ not
              . andP
                  [ assertEventType WorkerRestarted
                  , assertWorkerName "B"
                  ]
              )
        , testCase "restarts sub-routines that are not temporary"
          $ testCapatazStreamWithOptions
              []
              ( \supOptions -> supOptions
                { SUT.capatazRestartStrategy       = SUT.AllForOne
                , SUT.capatazWorkerTerminationOrder = SUT.OldestFirst
                }
              )
              ( \capataz -> do
                ioA     <- mkFailingSubRoutine 1

                -- This lockVar guarantees that workerB executes before workerA
                lockVar <- newEmptyMVar

                _workerA <- SUT.forkWorker
                  SUT.defWorkerOptions { SUT.workerName            = "A"
                                      , SUT.workerRestartStrategy = SUT.Permanent
                                      }
                  (forever $ readMVar lockVar >> ioA)
                  capataz

                _workerB <- SUT.forkWorker
                  SUT.defWorkerOptions { SUT.workerName            = "B"
                                      , SUT.workerRestartStrategy = SUT.Transient
                                      }
                  (putMVar lockVar () >> forever (threadDelay 10))
                  capataz

                return ()
              )
              [ andP
                [assertEventType WorkerRestarted, assertWorkerName "A"]
              , andP
                [assertEventType WorkerRestarted, assertWorkerName "B"]
              ]
              []
              Nothing
        ]
      ]
    ]

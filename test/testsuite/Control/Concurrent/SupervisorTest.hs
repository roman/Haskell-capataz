{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Control.Concurrent.SupervisorTest (tests) where

import Protolude

import qualified Data.Text as T

import Data.IORef       (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Text.Show.Pretty (ppShow)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar   (modifyTVar', newTVarIO, readTVar)

import qualified Control.Concurrent.Supervisor as SUT

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
  = InvalidSupervisorStatusReached
  | SupervisorStatusChanged
  | SupervisedChildTerminated
  | SupervisedChildStarted
  | SupervisedChildRestarted
  | SupervisedChildCompleted
  | SupervisedChildFailed
  | SupervisedChildCallbackExecuted
  | SupervisedChildrenTerminationStarted
  | SupervisedChildrenTerminationFinished
  | SupervisorFailed
  | SupervisorTerminated
  deriving (Show)

assertEventType :: EventType -> SUT.SupervisorEvent -> Bool
assertEventType evType ev = fetchRecordName ev == show evType

assertChildName :: Text -> SUT.SupervisorEvent -> Bool
assertChildName childName' ev = case ev of
  SUT.SupervisedChildRestarted { childName }  -> childName' == childName
  SUT.SupervisedChildFailed { childName }     -> childName' == childName
  SUT.SupervisedChildTerminated { childName } -> childName' == childName
  SUT.SupervisedChildStarted { childName }    -> childName' == childName
  _                                           -> False

assertErrorType :: Text -> SUT.SupervisorEvent -> Bool
assertErrorType errType ev = case ev of
  SUT.SupervisedChildFailed { childError } ->
    fetchRecordName childError == errType
  SUT.SupervisorFailed { supervisorError } ->
    fetchRecordName supervisorError == errType
  SUT.SupervisedChildCallbackExecuted { childCallbackError } ->
    case childCallbackError of
      Nothing            -> False
      Just originalError -> fetchRecordName originalError == errType
  _ -> False

assertCallbackType :: SUT.CallbackType -> SUT.SupervisorEvent -> Bool
assertCallbackType cbType ev = case ev of
  SUT.SupervisedChildFailed { childError } -> case fromException childError of
    Just SUT.ChildCallbackFailed { callbackType } -> cbType == callbackType
    _                                             -> False
  SUT.SupervisedChildCallbackExecuted { callbackType } ->
    cbType == callbackType
  _ -> False

assertRestartCount :: (Int -> Bool) -> SUT.SupervisorEvent -> Bool
assertRestartCount predFn ev = case ev of
  SUT.SupervisedChildRestarted { childRestartCount } ->
    predFn childRestartCount
  _ -> False

assertSupervisorStatusChanged
  :: SUT.SupervisorStatus -> SUT.SupervisorStatus -> SUT.SupervisorEvent -> Bool
assertSupervisorStatusChanged fromEv toEv ev = case ev of
  SUT.SupervisorStatusChanged { prevSupervisorStatus, newSupervisorStatus } ->
    fromEv == prevSupervisorStatus && toEv == newSupervisorStatus
  _ -> False

assertChildStarted :: Text -> SUT.SupervisorEvent -> Bool
assertChildStarted childName =
  andP [assertEventType SupervisedChildStarted, assertChildName childName]

assertChildTerminated :: Text -> SUT.SupervisorEvent -> Bool
assertChildTerminated childName =
  andP [assertEventType SupervisedChildTerminated, assertChildName childName]

assertSupervisorFailedWith :: Text -> SUT.SupervisorEvent -> Bool
assertSupervisorFailedWith errorName =
  andP [assertEventType SupervisorFailed, assertErrorType errorName]

--------------------------------------------------------------------------------

data RestartingChildError
  = RestartingChildError
  deriving (Show)

instance Exception RestartingChildError

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
        when shouldFail (throwIO RestartingChildError)

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

testSupervisorStreamWithOptions
  :: [SUT.SupervisorEvent -> Bool]
  -> (SUT.SupervisorOptions -> SUT.SupervisorOptions)
  -> (SUT.Supervisor -> IO ())
  -> [SUT.SupervisorEvent -> Bool]
  -> [SUT.SupervisorEvent -> Bool]
  -> Maybe (SUT.SupervisorEvent -> Bool)
  -> IO ()
testSupervisorStreamWithOptions preSetupAssertion optionModFn setupFn postSetupAssertions postTeardownAssertions mAllEventsAssertion
  = do

    eventStream     <- newTQueueIO
    accRef          <- newIORef []
    pendingCountVar <- newIORef
      ( sum $ fmap
        length
        [preSetupAssertion, postSetupAssertions, postTeardownAssertions]
      )

    supervisor <- SUT.forkSupervisor $ (optionModFn SUT.defSupervisorOptions)
      { SUT.notifyEvent = trackEvent accRef eventStream
      }

    runAssertions (eventStream, accRef)
                  pendingCountVar
                  preSetupAssertion
                  supervisor
    setupResult <- try (setupFn supervisor)

    case setupResult of
      Left  err -> assertFailure (show (err :: SomeException))
      Right _   -> do
        runAssertions (eventStream, accRef)
                      pendingCountVar
                      postSetupAssertions
                      supervisor

        void $ SUT.teardown supervisor
        runAssertions (eventStream, accRef)
                      pendingCountVar
                      postTeardownAssertions
                      supervisor

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
  runAssertions (eventStream, accRef) pendingCountVar assertions supervisor =
    do
      raceResult <- race
        (threadDelay 1000100)
        (readEventLoop eventStream pendingCountVar assertions)
      case raceResult of
        Left _ -> do
          events       <- reverse <$> readIORef accRef
          pendingCount <- readIORef pendingCountVar
          void $ SUT.teardown supervisor
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


testSupervisorStream
  :: [SUT.SupervisorEvent -> Bool]
  -> (SUT.Supervisor -> IO ())
  -> [SUT.SupervisorEvent -> Bool]
  -> [SUT.SupervisorEvent -> Bool]
  -> Maybe (SUT.SupervisorEvent -> Bool)
  -> IO ()
testSupervisorStream preSetupAssertions =
  testSupervisorStreamWithOptions preSetupAssertions identity

--------------------------------------------------------------------------------
-- Actual Tests

tests :: [TestTree]
tests
  = [ testGroup
      "supervisor without childSpecList"
      [ testCase "initialize and teardown works as expected"
          $ testSupervisorStream
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
      "supervisor with childSpecList"
      [ testCase "initialize and teardown works as expected"
          $ testSupervisorStreamWithOptions
              [ assertChildStarted "A"
              , assertChildStarted "B"
              , andP
                [ assertEventType SupervisorStatusChanged
                , assertSupervisorStatusChanged SUT.Initializing SUT.Running
                ]
              ]
              ( \supOptions -> supOptions
                { SUT.supervisorChildSpecList = [ SUT.defChildSpec
                                                  { SUT.childName   = "A"
                                                  , SUT.childAction = forever
                                                    (threadDelay 10001000)
                                                  }
                                                , SUT.defChildSpec
                                                  { SUT.childName   = "B"
                                                  , SUT.childAction = forever
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
              , assertEventType SupervisedChildrenTerminationStarted
              , assertChildTerminated "A"
              , assertChildTerminated "B"
              , assertEventType SupervisedChildrenTerminationFinished
              , andP
                [ assertEventType SupervisorStatusChanged
                , assertSupervisorStatusChanged SUT.Halting SUT.Halted
                ]
              ]
              Nothing
      ]
    , testCase
        "reports supervisor error when supervisor thread receives async exception"
      $ testSupervisorStream
          [ andP
              [ assertEventType SupervisorStatusChanged
              , assertSupervisorStatusChanged SUT.Initializing SUT.Running
              ]
          ]
          ( \SUT.Supervisor { supervisorAsync } -> do
            threadDelay 100 -- leave enough room for supervisor to start
            cancelWith supervisorAsync (ErrorCall "async exception")
          )
          [assertEventType SupervisorFailed]
          []
          Nothing
    , testCase
        "reports supervisor error when child retries violate restart intensity"
      $ do
          lockVar <- newEmptyMVar
          let (signalIntensityReached, waitTillIntensityReached) =
                (putMVar lockVar (), takeMVar lockVar)
          testSupervisorStreamWithOptions
            []
            ( \supOptions -> supOptions
              { SUT.onSupervisorIntensityReached = signalIntensityReached
              }
            )
            ( \supervisor -> do
              _childId <- SUT.forkChild SUT.defChildOptions
                                        (throwIO RestartingChildError)
                                        supervisor
              waitTillIntensityReached
            )
            [ assertEventType SupervisedChildFailed
            , assertEventType SupervisedChildFailed
            , assertEventType SupervisedChildFailed
            , assertSupervisorFailedWith "SupervisorIntensityReached"
            ]
            []
            Nothing
    , testGroup
      "single supervised IO sub-routine"
      [ testGroup
        "callbacks"
        [ testGroup
          "childOnCompletion"
          [ testCase "does execute callback when sub-routine is completed"
            $ testSupervisorStream
                []
                ( \supervisor -> do
                  _childId <- SUT.forkChild
                    ( SUT.defChildOptions
                      { SUT.childRestartStrategy = SUT.Temporary
                      }
                    )
                    (return ())
                    supervisor
                  return ()
                )
                [ andP
                  [ assertEventType SupervisedChildCallbackExecuted
                  , assertCallbackType SUT.OnCompletion
                  ]
                , assertEventType SupervisedChildCompleted
                ]
                []
                Nothing
          , testCase "does not execute callback when sub-routine fails"
            $ testSupervisorStream
                []
                ( \supervisor -> do
                  _childId <- SUT.forkChild
                    ( SUT.defChildOptions
                      { SUT.childRestartStrategy = SUT.Temporary
                      }
                    )
                    (throwIO RestartingChildError)
                    supervisor
                  return ()
                )
                [ andP
                  [ assertEventType SupervisedChildCallbackExecuted
                  , assertCallbackType SUT.OnFailure
                  ]
                , assertEventType SupervisedChildFailed
                ]
                [assertEventType SupervisorTerminated]
                ( Just $ not . andP
                  [ assertEventType SupervisedChildCallbackExecuted
                  , assertCallbackType SUT.OnCompletion
                  ]
                )
          , testCase "does not execute callback when sub-routine is terminated"
            $ testSupervisorStream
                []
                ( \supervisor -> do
                  childId <- SUT.forkChild
                    ( SUT.defChildOptions
                      { SUT.childRestartStrategy = SUT.Temporary
                      }
                    )
                    (forever $ threadDelay 1000100)
                    supervisor

                  _childId <- SUT.terminateChild
                    "testing onCompletion callback"
                    childId
                    supervisor
                  return ()
                )
                [assertEventType SupervisedChildTerminated]
                [assertEventType SupervisorTerminated]
                ( Just $ not . andP
                  [ assertEventType SupervisedChildCallbackExecuted
                  , assertCallbackType SUT.OnCompletion
                  ]
                )
          , testCase "treats as sub-routine failed if callback fails"
            $ testSupervisorStream
                []
                ( \supervisor -> do
                  _childId <- SUT.forkChild
                    ( SUT.defChildOptions
                      { SUT.childRestartStrategy = SUT.Temporary
                      , SUT.childOnCompletion    = throwIO TimeoutError
                      }
                    )
                    (return ())
                    supervisor

                  return ()
                )
                [ andP
                  [ assertEventType SupervisedChildCallbackExecuted
                  , assertCallbackType SUT.OnCompletion
                  , assertErrorType "TimeoutError"
                  ]
                , andP
                  [ assertEventType SupervisedChildFailed
                  , assertErrorType "ChildCallbackFailed"
                  ]
                ]
                []
                Nothing
          ]
        , testGroup
          "childOnFailure"
          [ testCase "does execute callback when sub-routine fails"
            $ testSupervisorStream
                []
                ( \supervisor -> do
                  _childId <- SUT.forkChild
                    ( SUT.defChildOptions
                      { SUT.childRestartStrategy = SUT.Temporary
                      }
                    )
                    (throwIO RestartingChildError)
                    supervisor
                  return ()
                )
                [ andP
                  [ assertEventType SupervisedChildCallbackExecuted
                  , assertCallbackType SUT.OnFailure
                  ]
                , assertEventType SupervisedChildFailed
                ]
                [assertEventType SupervisorTerminated]
                Nothing
          , testCase "does not execute callback when sub-routine is completed"
            $ testSupervisorStream
                []
                ( \supervisor -> do
                  _childId <- SUT.forkChild
                    ( SUT.defChildOptions
                      { SUT.childRestartStrategy = SUT.Temporary
                      }
                    )
                    (return ())
                    supervisor
                  return ()
                )
                [ andP
                  [ assertEventType SupervisedChildCallbackExecuted
                  , assertCallbackType SUT.OnCompletion
                  ]
                , assertEventType SupervisedChildCompleted
                ]
                []
                ( Just $ not . andP
                  [ assertEventType SupervisedChildCallbackExecuted
                  , assertCallbackType SUT.OnFailure
                  ]
                )
          , testCase "does not execute callback when sub-routine is terminated"
            $ testSupervisorStream
                []
                ( \supervisor -> do
                  childId <- SUT.forkChild
                    ( SUT.defChildOptions
                      { SUT.childRestartStrategy = SUT.Temporary
                      }
                    )
                    (forever $ threadDelay 1000100)
                    supervisor

                  SUT.terminateChild "testing onFailure callback"
                                     childId
                                     supervisor
                )
                [assertEventType SupervisedChildTerminated]
                []
                ( Just $ not . andP
                  [ assertEventType SupervisedChildCallbackExecuted
                  , assertCallbackType SUT.OnFailure
                  ]
                )
          , testCase "treats as sub-routine failed if callback fails"
            $ testSupervisorStream
                []
                ( \supervisor -> do
                  _childId <- SUT.forkChild
                    ( SUT.defChildOptions
                      { SUT.childRestartStrategy = SUT.Temporary
                      , SUT.childOnFailure       = const $ throwIO TimeoutError
                      }
                    )
                    (throwIO RestartingChildError)
                    supervisor

                  return ()
                )
                [ andP
                  [ assertEventType SupervisedChildCallbackExecuted
                  , assertCallbackType SUT.OnFailure
                  , assertErrorType "TimeoutError"
                  ]
                , andP
                  [ assertEventType SupervisedChildFailed
                  , assertErrorType "ChildCallbackFailed"
                  ]
                ]
                []
                Nothing
          ]
        , testGroup
          "childOnTermination"
          [ testCase
              "gets brutally killed when TimeoutSeconds termination policy is not met"
            $ testSupervisorStream
                []
                ( \supervisor -> do
                  childId <- SUT.forkChild
                    ( SUT.defChildOptions
                      { SUT.childRestartStrategy = SUT.Temporary
                      , SUT.childTerminationPolicy = SUT.TimeoutMillis 1
                      , SUT.childOnTermination = forever $ threadDelay 100100
                      }
                    )
                    (forever $ threadDelay 10001000)
                    supervisor

                  SUT.terminateChild "testing childOnTermination callback"
                                     childId
                                     supervisor
                )
                [ andP
                  [ assertEventType SupervisedChildCallbackExecuted
                  , assertCallbackType SUT.OnTermination
                  , assertErrorType "BrutallyTerminateChildException"
                  ]
                , andP
                  [ assertEventType SupervisedChildFailed
                  , assertErrorType "ChildCallbackFailed"
                  , assertCallbackType SUT.OnTermination
                  ]
                ]
                []
                Nothing
          , testCase "does execute callback when sub-routine is terminated"
            $ testSupervisorStream
                []
                ( \supervisor -> do
                  childId <- SUT.forkChild
                    ( SUT.defChildOptions
                      { SUT.childRestartStrategy = SUT.Temporary
                      }
                    )
                    (forever $ threadDelay 1000100)
                    supervisor

                  SUT.terminateChild "testing childOnTermination callback"
                                     childId
                                     supervisor
                )
                [ andP
                  [ assertEventType SupervisedChildCallbackExecuted
                  , assertCallbackType SUT.OnTermination
                  ]
                , assertEventType SupervisedChildTerminated
                ]
                [assertEventType SupervisorTerminated]
                Nothing
          , testCase "does not execute callback when sub-routine is completed"
            $ testSupervisorStream
                []
                ( \supervisor -> do
                  _childId <- SUT.forkChild
                    ( SUT.defChildOptions
                      { SUT.childRestartStrategy = SUT.Temporary
                      }
                    )
                    (return ())
                    supervisor
                  return ()
                )
                [assertEventType SupervisedChildCompleted]
                []
                ( Just $ not . andP
                  [ assertEventType SupervisedChildCallbackExecuted
                  , assertCallbackType SUT.OnTermination
                  ]
                )
          , testCase "does not execute callback when sub-routine fails"
            $ testSupervisorStream
                []
                ( \supervisor -> do
                  _childId <- SUT.forkChild
                    ( SUT.defChildOptions
                      { SUT.childRestartStrategy = SUT.Temporary
                      }
                    )
                    (throwIO (ErrorCall "surprise!"))
                    supervisor
                  return ()
                )
                [assertEventType SupervisedChildFailed]
                []
                ( Just $ not . andP
                  [ assertEventType SupervisedChildCallbackExecuted
                  , assertCallbackType SUT.OnTermination
                  ]
                )
          , testCase "treats as sub-routine failed if callback fails"
            $ testSupervisorStream
                []
                ( \supervisor -> do
                  childId <- SUT.forkChild
                    ( SUT.defChildOptions
                      { SUT.childRestartStrategy = SUT.Temporary
                      , SUT.childOnTermination   = throwIO TimeoutError
                      }
                    )
                    (forever $ threadDelay 10001000)
                    supervisor

                  SUT.terminateChild "testing childOnTermination callback"
                                     childId
                                     supervisor
                )
                [ andP
                  [ assertEventType SupervisedChildCallbackExecuted
                  , assertCallbackType SUT.OnTermination
                  , assertErrorType "TimeoutError"
                  ]
                , andP
                  [ assertEventType SupervisedChildFailed
                  , assertErrorType "ChildCallbackFailed"
                  ]
                ]
                []
                Nothing
          ]
        ]
      , testGroup
        "with transient strategy"
        [ testCase "does not restart on completion" $ testSupervisorStream
          []
          ( \supervisor -> do
            _childId <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Transient }
              (return ())
              supervisor
            return ()
          )
          [ assertEventType SupervisedChildStarted
          , assertEventType SupervisedChildCompleted
          ]
          [assertEventType SupervisorTerminated]
          (Just $ not . assertEventType SupervisedChildRestarted)
        , testCase "does not restart on termination" $ testSupervisorStream
          []
          ( \supervisor -> do
            childId <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Transient }
              (forever $ threadDelay 1000100)
              supervisor
            SUT.terminateChild "termination test (1)" childId supervisor
          )
          [assertEventType SupervisedChildTerminated]
          [assertEventType SupervisorTerminated]
          (Just $ not . assertEventType SupervisedChildRestarted)
        , testCase "does restart on failure" $ testSupervisorStream
          []
          ( \supervisor -> do
            subRoutineAction <- mkFailingSubRoutine 1
            _childId         <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Transient }
              subRoutineAction
              supervisor
            return ()
          )
          [ assertEventType SupervisedChildStarted
          , assertEventType SupervisedChildFailed
          , andP
            [ assertEventType SupervisedChildRestarted
            , assertRestartCount (== 1)
            ]
          ]
          []
          Nothing
        , testCase "does increase restart count on multiple failures"
          $ testSupervisorStream
              []
              ( \supervisor -> do
                subRoutineAction <- mkFailingSubRoutine 2
                _childId         <- SUT.forkChild
                  SUT.defChildOptions { SUT.childRestartStrategy = SUT.Transient
                                      }
                  subRoutineAction
                  supervisor
                return ()
              )
              [ andP
                [ assertEventType SupervisedChildRestarted
                , assertRestartCount (== 1)
                ]
              , andP
                [ assertEventType SupervisedChildRestarted
                , assertRestartCount (== 2)
                ]
              ]
              []
              Nothing
        ]
      , testGroup
        "with permanent strategy"
        [ testCase "does restart on completion" $ testSupervisorStream
          []
          ( \supervisor -> do
            subRoutineAction <- mkCompletingOnceSubRoutine
            _childId         <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Permanent }
              subRoutineAction
              supervisor
            return ()
          )
          [ assertEventType SupervisedChildStarted
          , assertEventType SupervisedChildCompleted
          , assertEventType SupervisedChildRestarted
          ]
          [assertEventType SupervisorTerminated]
          Nothing
        , testCase "does not increase restart count on multiple completions"
          $ testSupervisorStream
              []
              ( \supervisor -> do
            -- Note the number is two (2) given the assertion list has two `SupervisedChildRestarted` assertions
                let expectedRestartCount = 2
                subRoutineAction <- mkCompletingBeforeNRestartsSubRoutine
                  expectedRestartCount
                _childId <- SUT.forkChild
                  SUT.defChildOptions { SUT.childRestartStrategy = SUT.Permanent
                                      }
                  subRoutineAction
                  supervisor
                return ()
              )
              [ andP
                [ assertEventType SupervisedChildRestarted
                , assertRestartCount (== 1)
                ]
              , andP
                [ assertEventType SupervisedChildRestarted
                , assertRestartCount (== 1)
                ]
              ]
              []
              Nothing
        , testCase "does restart on termination" $ testSupervisorStream
          []
          ( \supervisor -> do
            childId <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Permanent }
              (forever $ threadDelay 10001000)
              supervisor
            SUT.terminateChild "testing termination (1)" childId supervisor
          )
          [ assertEventType SupervisedChildTerminated
          , assertEventType SupervisedChildRestarted
          ]
          []
          Nothing
        , testCase "does increase restart count on multiple terminations" $ do
          terminationCountVar <- newTVarIO (0 :: Int)
          let signalChildTermination =
                atomically (modifyTVar' terminationCountVar (+ 1))
              waitChildTermination i = atomically $ do
                n <- readTVar terminationCountVar
                when (n /= i) retry
          testSupervisorStream
            []
            ( \supervisor -> do
              childId <- SUT.forkChild
                SUT.defChildOptions
                  { SUT.childRestartStrategy = SUT.Permanent
                  , SUT.childOnTermination   = signalChildTermination
                  }
                (forever $ threadDelay 10001000)
                supervisor

              SUT.terminateChild "testing termination (1)" childId supervisor
              waitChildTermination 1
              SUT.terminateChild "testing termination (2)" childId supervisor
              waitChildTermination 2
            )
            [ assertEventType SupervisedChildTerminated
            , andP
              [ assertEventType SupervisedChildRestarted
              , assertRestartCount (== 1)
              ]
            , assertEventType SupervisedChildTerminated
            , andP
              [ assertEventType SupervisedChildRestarted
              , assertRestartCount (== 2)
              ]
            ]
            []
            Nothing
        , testCase "does restart on failure" $ testSupervisorStream
          []
          ( \supervisor -> do
            subRoutineAction <- mkFailingSubRoutine 1
            _childId         <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Permanent }
              subRoutineAction
              supervisor
            return ()
          )
          [ assertEventType SupervisedChildStarted
          , assertEventType SupervisedChildFailed
          , andP
            [ assertEventType SupervisedChildRestarted
            , assertRestartCount (== 1)
            ]
          ]
          []
          Nothing
        , testCase "does increase restart count on multiple failures"
          $ testSupervisorStream
              []
              ( \supervisor -> do
                subRoutineAction <- mkFailingSubRoutine 2
                _childId         <- SUT.forkChild
                  SUT.defChildOptions { SUT.childRestartStrategy = SUT.Permanent
                                      }
                  subRoutineAction
                  supervisor
                return ()
              )
              [ andP
                [ assertEventType SupervisedChildRestarted
                , assertRestartCount (== 1)
                ]
              , andP
                [ assertEventType SupervisedChildRestarted
                , assertRestartCount (== 2)
                ]
              ]
              []
              Nothing
        ]
      , testGroup
        "with temporary strategy"
        [ testCase "does not restart on completion" $ testSupervisorStream
          []
          ( \supervisor -> do
            _childId <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Temporary }
              (return ())
              supervisor
            return ()
          )
          [ assertEventType SupervisedChildStarted
          , assertEventType SupervisedChildCompleted
          ]
          [assertEventType SupervisorTerminated]
          (Just $ not . assertEventType SupervisedChildRestarted)
        , testCase "does not restart on termination" $ testSupervisorStream
          []
          ( \supervisor -> do
            childId <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Temporary }
              (forever $ threadDelay 1000100)
              supervisor
            SUT.terminateChild "termination test (1)" childId supervisor
            threadDelay 100
          )
          [ assertEventType SupervisedChildStarted
          , assertEventType SupervisedChildTerminated
          ]
          [assertEventType SupervisorTerminated]
          (Just $ not . assertEventType SupervisedChildRestarted)
        , testCase "does not restart on failure" $ testSupervisorStream
          []
          ( \supervisor -> do
            _childId <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Temporary }
              (panic "child failed!")
              supervisor
            threadDelay 100
          )
          [ assertEventType SupervisedChildStarted
          , assertEventType SupervisedChildFailed
          ]
          [assertEventType SupervisorTerminated]
          (Just $ not . assertEventType SupervisedChildRestarted)
        ]
      ]
    , testGroup
      "multiple supervised IO sub-routines"
      [ testCase "terminates all supervised sub-routines on teardown"
        $ testSupervisorStream
            []
            ( \supervisor -> do
              _childA <- SUT.forkChild
                SUT.defChildOptions { SUT.childName            = "A"
                                    , SUT.childRestartStrategy = SUT.Permanent
                                    }
                (forever $ threadDelay 1000100)
                supervisor


              _childB <- SUT.forkChild
                SUT.defChildOptions { SUT.childName            = "B"
                                    , SUT.childRestartStrategy = SUT.Permanent
                                    }
                (forever $ threadDelay 1000100)
                supervisor

              return ()
            )
            [ andP [assertEventType SupervisedChildStarted, assertChildName "A"]
            , andP [assertEventType SupervisedChildStarted, assertChildName "B"]
            ]
            [ andP
              [assertEventType SupervisedChildTerminated, assertChildName "A"]
            , andP
              [assertEventType SupervisedChildTerminated, assertChildName "B"]
            , assertEventType SupervisorTerminated
            ]
            Nothing
      , testGroup
        "with one for one supervisor restart strategy"
        [ testCase "restarts failing sub-routine only"
            $ testSupervisorStreamWithOptions
                []
                ( \supOptions ->
                  supOptions { SUT.supervisorRestartStrategy = SUT.OneForOne }
                )
                ( \supervisor -> do
                  _childA <- SUT.forkChild
                    SUT.defChildOptions
                      { SUT.childName            = "A"
                      , SUT.childRestartStrategy = SUT.Temporary
                      }
                    (forever $ threadDelay 1000100)
                    supervisor

                  ioB     <- mkFailingSubRoutine 1

                  _childB <- SUT.forkChild
                    SUT.defChildOptions
                      { SUT.childName            = "B"
                      , SUT.childRestartStrategy = SUT.Permanent
                      }
                    (forever $ ioB >> threadDelay 1000100)
                    supervisor

                  return ()
                )
                [ andP
                    [ assertEventType SupervisedChildRestarted
                    , assertChildName "B"
                    ]
                ]
                []
                ( Just
                $ not
                . andP
                    [ assertEventType SupervisedChildRestarted
                    , assertChildName "A"
                    ]
                )
        ]
      , testGroup
        "with all for one supervisor restart strategy with newest first order"
        [ testCase "does terminate all other children that did not fail"
          $ testSupervisorStreamWithOptions
              []
              ( \supOptions -> supOptions
                { SUT.supervisorRestartStrategy       = SUT.AllForOne
                , SUT.supervisorChildTerminationOrder = SUT.OldestFirst
                }
              )
              ( \supervisor -> do
              -- This lockVar guarantees that childB executes before childA
                lockVar <- newEmptyMVar

                ioA     <- mkFailingSubRoutine 1

                _childA <- SUT.forkChild
                  SUT.defChildOptions { SUT.childName            = "A"
                                      , SUT.childRestartStrategy = SUT.Permanent
                                      }
                  (forever $ readMVar lockVar >> ioA)
                  supervisor

                _childB <- SUT.forkChild
                  SUT.defChildOptions { SUT.childName            = "B"
                                      , SUT.childRestartStrategy = SUT.Permanent
                                      }
                  (putMVar lockVar () >> forever (threadDelay 10))
                  supervisor

                return ()
              )
              [ andP
                [assertEventType SupervisedChildStarted, assertChildName "A"]
              , andP
                [assertEventType SupervisedChildStarted, assertChildName "B"]
              , andP
                [assertEventType SupervisedChildFailed, assertChildName "A"]
              , andP
                [assertEventType SupervisedChildRestarted, assertChildName "A"]
              , andP
                [assertEventType SupervisedChildTerminated, assertChildName "B"]
              , andP
                [assertEventType SupervisedChildRestarted, assertChildName "B"]
              ]
              []
              Nothing
        , testCase "does not restart sub-routines that are temporary"
          $ testSupervisorStreamWithOptions
              []
              ( \supOptions -> supOptions
                { SUT.supervisorRestartStrategy       = SUT.AllForOne
                , SUT.supervisorChildTerminationOrder = SUT.OldestFirst
                }
              )
              ( \supervisor -> do
                lockVar <- newEmptyMVar

                ioA     <- mkFailingSubRoutine 1

                _childA <- SUT.forkChild
                  SUT.defChildOptions { SUT.childName            = "A"
                                      , SUT.childRestartStrategy = SUT.Permanent
                                      }
                  (forever $ readMVar lockVar >> ioA)
                  supervisor

                _childB <- SUT.forkChild
                  SUT.defChildOptions { SUT.childName            = "B"
                                      , SUT.childRestartStrategy = SUT.Temporary
                                      }
                  (putMVar lockVar () >> forever (threadDelay 10))
                  supervisor

                return ()
              )
              [ andP
                [assertEventType SupervisedChildStarted, assertChildName "A"]
              , andP
                [assertEventType SupervisedChildStarted, assertChildName "B"]
              , andP
                [assertEventType SupervisedChildFailed, assertChildName "A"]
              , andP
                [assertEventType SupervisedChildRestarted, assertChildName "A"]
              , andP
                [assertEventType SupervisedChildTerminated, assertChildName "B"]
              ]
              []
              ( Just
              $ not
              . andP
                  [ assertEventType SupervisedChildRestarted
                  , assertChildName "B"
                  ]
              )
        , testCase "restarts sub-routines that are not temporary"
          $ testSupervisorStreamWithOptions
              []
              ( \supOptions -> supOptions
                { SUT.supervisorRestartStrategy       = SUT.AllForOne
                , SUT.supervisorChildTerminationOrder = SUT.NewestFirst
                }
              )
              ( \supervisor -> do
                ioA     <- mkFailingSubRoutine 1

                lockVar <- newEmptyMVar

                _childA <- SUT.forkChild
                  SUT.defChildOptions { SUT.childName            = "A"
                                      , SUT.childRestartStrategy = SUT.Permanent
                                      }
                  (forever $ readMVar lockVar >> ioA)
                  supervisor

                _childB <- SUT.forkChild
                  SUT.defChildOptions { SUT.childName            = "B"
                                      , SUT.childRestartStrategy = SUT.Transient
                                      }
                  (putMVar lockVar () >> forever (threadDelay 10))
                  supervisor

                return ()
              )
              [ andP
                [assertEventType SupervisedChildRestarted, assertChildName "B"]
              , andP
                [assertEventType SupervisedChildRestarted, assertChildName "A"]
              ]
              []
              Nothing
        ]
      , testGroup
        "with all for one supervisor restart strategy with oldest first order"
        [ testCase "does not restart sub-routines that are temporary"
          $ testSupervisorStreamWithOptions
              []
              ( \supOptions -> supOptions
                { SUT.supervisorRestartStrategy       = SUT.AllForOne
                , SUT.supervisorChildTerminationOrder = SUT.OldestFirst
                }
              )
              ( \supervisor -> do
                ioA     <- mkFailingSubRoutine 1

                -- This lockVar guarantees that childB executes before childA
                lockVar <- newEmptyMVar

                _childA <- SUT.forkChild
                  SUT.defChildOptions { SUT.childName            = "A"
                                      , SUT.childRestartStrategy = SUT.Permanent
                                      }
                  (forever $ readMVar lockVar >> ioA)
                  supervisor

                _childB <- SUT.forkChild
                  SUT.defChildOptions { SUT.childName            = "B"
                                      , SUT.childRestartStrategy = SUT.Temporary
                                      }
                  (putMVar lockVar () >> forever (threadDelay 10))
                  supervisor

                return ()
              )
              [ andP
                  [ assertEventType SupervisedChildRestarted
                  , assertChildName "A"
                  ]
              ]
              []
              ( Just
              $ not
              . andP
                  [ assertEventType SupervisedChildRestarted
                  , assertChildName "B"
                  ]
              )
        , testCase "restarts sub-routines that are not temporary"
          $ testSupervisorStreamWithOptions
              []
              ( \supOptions -> supOptions
                { SUT.supervisorRestartStrategy       = SUT.AllForOne
                , SUT.supervisorChildTerminationOrder = SUT.OldestFirst
                }
              )
              ( \supervisor -> do
                ioA     <- mkFailingSubRoutine 1

                -- This lockVar guarantees that childB executes before childA
                lockVar <- newEmptyMVar

                _childA <- SUT.forkChild
                  SUT.defChildOptions { SUT.childName            = "A"
                                      , SUT.childRestartStrategy = SUT.Permanent
                                      }
                  (forever $ readMVar lockVar >> ioA)
                  supervisor

                _childB <- SUT.forkChild
                  SUT.defChildOptions { SUT.childName            = "B"
                                      , SUT.childRestartStrategy = SUT.Transient
                                      }
                  (putMVar lockVar () >> forever (threadDelay 10))
                  supervisor

                return ()
              )
              [ andP
                [assertEventType SupervisedChildRestarted, assertChildName "A"]
              , andP
                [assertEventType SupervisedChildRestarted, assertChildName "B"]
              ]
              []
              Nothing
        ]
      ]
    ]

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main where

import Protolude

import Data.IORef       (atomicModifyIORef', newIORef, readIORef)
import Text.Show.Pretty (ppShow)

import Test.Tasty                   (TestTree, defaultMainWithIngredients, testGroup)
import Test.Tasty.HUnit             (assertBool, assertFailure, testCase)
import Test.Tasty.Ingredients.Rerun (rerunningTests)
import Test.Tasty.Runners           (consoleTestReporter, listingTests)

-- import Text.Show.Pretty (pPrint)

import qualified Data.Text as T

import qualified Control.Concurrent.Supervisor as SUT

main :: IO ()
main = defaultMainWithIngredients
  [rerunningTests [listingTests, consoleTestReporter]]
  (testGroup "supervisor" tests)

--------------------------------------------------------------------------------
-- Util

fetchRecordName :: Show a => a -> Text
fetchRecordName = T.takeWhile (/= ' ') . show

andP :: [a -> Bool] -> a -> Bool
andP predList a = all ($ a) predList

orP :: [a -> Bool] -> a -> Bool
orP predList a = any ($ a) predList

--------------------------------------------------------------------------------
-- Assertions and Testers

combineAssertions
  :: [[SUT.SupervisorEvent] -> IO ()] -> [SUT.SupervisorEvent] -> IO ()
combineAssertions assertions events =
  mapM_ (\assertFn -> assertFn events) assertions

assertInOrder :: [SUT.SupervisorEvent -> Bool] -> [SUT.SupervisorEvent] -> IO ()
assertInOrder assertions0 events0 =
  let loop assertions events = case (assertions, events) of
        ([], _) -> return ()
        (assertionFn:assertions', event:events')
          | assertionFn event -> loop assertions' events'
          | otherwise         -> loop assertions events'
        (assertions', []) -> assertFailure
          (  "Expected all assertions to match, but didn't ("
          <> show (length assertions')
          <> " assertions remaining, "
          <> show (length events0)
          <> " events tried)\n"
          <> ppShow (zip ([0 ..] :: [Int]) events0)
          )
  in  loop assertions0 events0

assertAll :: (SUT.SupervisorEvent -> Bool) -> [SUT.SupervisorEvent] -> IO ()
assertAll predFn events = if all predFn events
  then return ()
  else assertFailure
    (  "Expected all events to match predicate, but didn't ("
    <> show (length events)
    <> " events tried)\n"
    <> ppShow (zip ([0 ..] :: [Int]) events)
    )

data EventType
  = InvalidSupervisorStatusReached
  | SupervisorStatusChanged
  | SupervisedChildTerminated
  | SupervisedChildStarted
  | SupervisedChildRestarted
  | SupervisedChildCompleted
  | SupervisedChildFailed
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


data RestartingChildError
  = RestartingChildError
  deriving (Show)

instance Exception RestartingChildError

data TimeoutError
  = TimeoutError
  deriving (Show)

instance Exception TimeoutError

failingChild :: Int -> IO (IO ())
failingChild failCount = do
  countRef <- newIORef failCount
  return $ do
    shouldFail <- atomicModifyIORef' countRef
                                     (\count -> (pred count, count > 0))
    if shouldFail then throwIO RestartingChildError else threadDelay 1000100

completingChild :: Int -> Int -> IO (IO ())
completingChild execCount delayMicros = do
  countRef <- newIORef execCount
  return $ do
    shouldComplete <- atomicModifyIORef' countRef
                                         (\count -> (pred count, count > 0))
    if shouldComplete
      then threadDelay delayMicros
      else forever $ threadDelay 1000100

testSupervisorWithOptions
  :: [Char]
  -> ([SUT.SupervisorEvent] -> IO ())
  -> (SUT.SupervisorOptions -> SUT.SupervisorOptions)
  -> (SUT.Supervisor -> IO ())
  -> TestTree
testSupervisorWithOptions testCaseStr assertionsFn optionModFn setupFn =
  testCase testCaseStr $ do
    accRef     <- newIORef []
    supervisor <- SUT.forkSupervisor $ (optionModFn SUT.defSupervisorOptions)
      { SUT.notifyEvent = trackEvent accRef
      }
    result :: Either SomeException () <- try
      (setupFn supervisor `finally` SUT.teardown supervisor)
    case result of
      Left  err -> assertFailure (show err)
      Right _   -> do
        events <- readIORef accRef
        assertionsFn (reverse events)
 where
  trackEvent accRef ev = atomicModifyIORef' accRef (\old -> (ev : old, ()))

testSupervisor
  :: [Char]
  -> ([SUT.SupervisorEvent] -> IO ())
  -> (SUT.Supervisor -> IO ())
  -> TestTree
testSupervisor testCaseStr assertionsFn =
  testSupervisorWithOptions testCaseStr assertionsFn identity

--------------------------------------------------------------------------------
-- Actual Tests

tests :: [TestTree]
tests
  = [
      testGroup "supervisor without childSpecList"
      [
        testSupervisor
        "initialize and teardown works as expected"
        ( assertInOrder
          [ andP
            [ assertEventType SupervisorStatusChanged
            , assertSupervisorStatusChanged SUT.Initializing SUT.Running
            ]
          , andP
            [ assertEventType SupervisorStatusChanged
            , assertSupervisorStatusChanged SUT.Running SUT.Halted
            ]
          ]
        )
        (\_supervisor -> threadDelay 500)
      ]
    , testGroup "supervisor with childSpecList"
      [
        testSupervisorWithOptions
        "initialize and teardown works as expected"
        ( assertInOrder
          [ andP
            [
              assertEventType SupervisedChildStarted
            , assertChildName "A"
            ]
          , andP
            [
              assertEventType SupervisedChildStarted
            , assertChildName "B"
            ]
          , andP
            [ assertEventType SupervisorStatusChanged
            , assertSupervisorStatusChanged SUT.Initializing SUT.Running
            ]
          , assertEventType SupervisedChildrenTerminationStarted
          , andP
            [
              assertEventType SupervisedChildTerminated
            , assertChildName "A"
            ]
          , andP
            [
              assertEventType SupervisedChildTerminated
            , assertChildName "B"
            ]
          , assertEventType SupervisedChildrenTerminationFinished
          , andP
            [ assertEventType SupervisorStatusChanged
            , assertSupervisorStatusChanged SUT.Running SUT.Halted
            ]
          ]
        )
        (\supOptions ->
           supOptions {
            SUT.supervisorChildSpecList =
                [ SUT.defChildSpec
                  { SUT.childName = "A"
                  , SUT.childAction = (forever $ threadDelay 1000100) }
                , SUT.defChildSpec
                  { SUT.childName = "B"
                  , SUT.childAction = (forever $ threadDelay 1000100) }
                ]
            })
        (\_supervisor -> threadDelay 500)
      ]
    , testSupervisor
      "reports supervisor error when supervisor thread receives async exception"
      (assertInOrder [assertEventType SupervisorFailed])
      ( \SUT.Supervisor { supervisorAsync } -> do
        threadDelay 100
        cancelWith supervisorAsync (ErrorCall "async exception")
      )
    , testSupervisor
      "reports supervisor error when child retries violate restart intesity"
      ( assertInOrder
        [ assertEventType SupervisedChildFailed
        , assertEventType SupervisedChildFailed
        , assertEventType SupervisedChildFailed
        , andP
          [ assertEventType SupervisorFailed
          , assertErrorType "SupervisorIntensityReached"
          ]
        ]
      )
      ( \supervisor -> do
        childSubRoutine <- failingChild 3
        _childId <- SUT.forkChild SUT.defChildOptions childSubRoutine supervisor
        threadDelay 500
      )
    , testGroup
      "single supervised IO sub-routine"
      [ testGroup
        "callbacks"
        [ testGroup
          "childOnCompletion"
          [ testSupervisor
            "does execute callback when sub-routine is completed"
            (assertInOrder [assertEventType SupervisedChildCompleted])
            ( \supervisor -> do
              callbackVar <- newEmptyMVar
              _childId    <- SUT.forkChild
                ( SUT.defChildOptions
                  { SUT.childRestartStrategy = SUT.Temporary
                  , SUT.childOnCompletion    = putMVar callbackVar ()
                  }
                )
                (return ())
                supervisor
              race_ (threadDelay 100 >> throwIO TimeoutError)
                    (takeMVar callbackVar)
            )
          , testSupervisor
            "does not execute callback when sub-routine fails"
            (assertInOrder [assertEventType SupervisedChildFailed])
            ( \supervisor -> do
              callbackVar <- newMVar ()
              _childId    <- SUT.forkChild
                ( SUT.defChildOptions
                  { SUT.childRestartStrategy = SUT.Temporary
                  , SUT.childOnCompletion    = takeMVar callbackVar
                  }
                )
                (throwIO RestartingChildError)
                supervisor

              threadDelay 100
              wasEmpty <- isEmptyMVar callbackVar
              assertBool
                "Expecting childOnCompletion to not get called, but it was"
                (not wasEmpty)
            )
          , testSupervisor
            "does not execute callback when sub-routine is terminated"
            (assertInOrder [assertEventType SupervisedChildTerminated])
            ( \supervisor -> do
              callbackVar <- newMVar ()
              childId     <- SUT.forkChild
                ( SUT.defChildOptions
                  { SUT.childRestartStrategy = SUT.Temporary
                  , SUT.childOnCompletion    = takeMVar callbackVar
                  }
                )
                (forever $ threadDelay 1000100)
                supervisor

              SUT.terminateChild "testing onCompletion callback"
                                 childId
                                 supervisor
              threadDelay 100

              wasEmpty <- isEmptyMVar callbackVar
              assertBool
                "Expecting childOnCompletion to not get called, but it was"
                (not wasEmpty)
            )
          , testSupervisor
            "treats as sub-routine failed if callback fails"
            ( assertInOrder
              [ andP
                  [ assertEventType SupervisedChildFailed
                  , assertErrorType "ChildCallbackFailed"
                  ]
              ]
            )
            ( \supervisor -> do
              _childId <- SUT.forkChild
                ( SUT.defChildOptions
                  { SUT.childRestartStrategy = SUT.Temporary
                  , SUT.childOnCompletion    = throwIO RestartingChildError
                  }
                )
                (return ())
                supervisor
              threadDelay 100
            )
          ]
        , testGroup
          "childOnFailure"
          [ testSupervisor
            "does execute callback when sub-routine fails"
            (assertInOrder [assertEventType SupervisedChildFailed])
            ( \supervisor -> do
              callbackVar <- newEmptyMVar
              _childId    <- SUT.forkChild
                ( SUT.defChildOptions
                  { SUT.childRestartStrategy = SUT.Temporary
                  , SUT.childOnFailure       = const $ putMVar callbackVar ()
                  }
                )
                (throwIO RestartingChildError)
                supervisor
              race_ (threadDelay 100 >> throwIO TimeoutError)
                    (takeMVar callbackVar)
            )
          , testSupervisor
            "does not execute callback when sub-routine is completed"
            (assertInOrder [assertEventType SupervisedChildCompleted])
            ( \supervisor -> do
              callbackVar <- newMVar ()
              _childId    <- SUT.forkChild
                ( SUT.defChildOptions
                  { SUT.childRestartStrategy = SUT.Temporary
                  , SUT.childOnFailure       = const $ takeMVar callbackVar
                  }
                )
                (return ())
                supervisor

              threadDelay 100
              wasEmpty <- isEmptyMVar callbackVar
              assertBool
                "Expecting childOnFailure to not get called, but it was"
                (not wasEmpty)
            )
          , testSupervisor
            "does not execute callback when sub-routine is terminated"
            (assertInOrder [assertEventType SupervisedChildTerminated])
            ( \supervisor -> do
              callbackVar <- newMVar ()
              childId     <- SUT.forkChild
                ( SUT.defChildOptions
                  { SUT.childRestartStrategy = SUT.Temporary
                  , SUT.childOnFailure       = const $ takeMVar callbackVar
                  }
                )
                (forever $ threadDelay 1000100)
                supervisor

              SUT.terminateChild "testing onFailure callback" childId supervisor
              threadDelay 100

              wasEmpty <- isEmptyMVar callbackVar
              assertBool
                "Expecting childOnFailure to not get called, but it was"
                (not wasEmpty)
            )
          , testSupervisor
            "treats as sub-routine failed if callback fails"
            ( assertInOrder
              [ andP
                  [ assertEventType SupervisedChildFailed
                  , assertErrorType "ChildCallbackFailed"
                  ]
              ]
            )
            ( \supervisor -> do
              _childId <- SUT.forkChild
                ( SUT.defChildOptions
                  { SUT.childRestartStrategy = SUT.Temporary
                  , SUT.childOnFailure = const $ throwIO RestartingChildError
                  }
                )
                (throwIO (ErrorCall "surprise"))
                supervisor
              threadDelay 100
            )
          ]
        , testGroup
          "childOnTermination"
          [ testSupervisor
            "does execute callback when sub-routine is terminated"
            (assertInOrder [assertEventType SupervisedChildTerminated])
            ( \supervisor -> do
              callbackVar <- newEmptyMVar
              childId     <- SUT.forkChild
                ( SUT.defChildOptions
                  { SUT.childRestartStrategy = SUT.Temporary
                  , SUT.childOnTermination   = putMVar callbackVar ()
                  }
                )
                (forever $ threadDelay 1000100)
                supervisor

              SUT.terminateChild "testing childOnTermination callback"
                                 childId
                                 supervisor
              race_ (threadDelay 100 >> throwIO TimeoutError)
                    (takeMVar callbackVar)
            )
          , testSupervisor
            "does not execute callback when sub-routine is completed"
            (assertInOrder [assertEventType SupervisedChildCompleted])
            ( \supervisor -> do
              callbackVar <- newMVar ()
              _childId    <- SUT.forkChild
                ( SUT.defChildOptions
                  { SUT.childRestartStrategy = SUT.Temporary
                  , SUT.childOnTermination   = takeMVar callbackVar
                  }
                )
                (return ())
                supervisor

              threadDelay 100
              wasEmpty <- isEmptyMVar callbackVar
              assertBool
                "Expecting childOnTermination to not get called, but it was"
                (not wasEmpty)
            )
          , testSupervisor
            "does not execute callback when sub-routine fails"
            (assertInOrder [assertEventType SupervisedChildFailed])
            ( \supervisor -> do
              callbackVar <- newMVar ()
              _childId    <- SUT.forkChild
                ( SUT.defChildOptions
                  { SUT.childRestartStrategy = SUT.Temporary
                  , SUT.childOnTermination   = takeMVar callbackVar
                  }
                )
                (throwIO (ErrorCall "surprise!"))
                supervisor

              threadDelay 100
              wasEmpty <- isEmptyMVar callbackVar
              assertBool
                "Expecting childOnTermination to not get called, but it was"
                (not wasEmpty)
            )
          , testSupervisor
            "treats as sub-routine failed if callback fails"
            ( assertInOrder
              [ andP
                  [ assertEventType SupervisedChildFailed
                  , assertErrorType "ChildCallbackFailed"
                  ]
              ]
            )
            ( \supervisor -> do
              childId <- SUT.forkChild
                ( SUT.defChildOptions
                  { SUT.childRestartStrategy = SUT.Temporary
                  , SUT.childOnTermination   = throwIO RestartingChildError
                  }
                )
                (forever $ threadDelay 10001000)
                supervisor

              SUT.terminateChild "testing childOnTermination callback"
                                 childId
                                 supervisor
              threadDelay 100
            )
          ]
        ]
      , testGroup
        "with transient strategy"
        [ testSupervisor
          "does not restart on completion"
          ( combineAssertions
            [ assertInOrder
              [ assertEventType SupervisedChildStarted
              , assertEventType SupervisedChildCompleted
              , assertEventType SupervisorTerminated
              ]
            , assertAll (not . assertEventType SupervisedChildRestarted)
            ]
          )
          ( \supervisor -> do
            _childId <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Transient }
              (return ())
              supervisor
            threadDelay 100
          )
        , testSupervisor
          "does not restart on termination"
          ( combineAssertions
            [ assertInOrder
              [ assertEventType SupervisedChildTerminated
              , assertEventType SupervisorTerminated
              ]
            , assertAll (not . assertEventType SupervisedChildRestarted)
            ]
          )
          ( \supervisor -> do
            childId <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Transient }
              (forever $ threadDelay 1000100)
              supervisor
            SUT.terminateChild "termination test (1)" childId supervisor
            threadDelay 500
          )
        , testSupervisor
          "does restart on failure"
          ( assertInOrder
            [ assertEventType SupervisedChildStarted
            , assertEventType SupervisedChildFailed
            , andP
              [ assertEventType SupervisedChildRestarted
              , assertRestartCount (== 1)
              ]
            ]
          )
          ( \supervisor -> do
            childAction <- failingChild 1
            _childId    <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Transient }
              childAction
              supervisor
            threadDelay 100
          )
        , testSupervisor
          "does increase restart count on multiple failures"
          ( assertInOrder
            [ andP
              [ assertEventType SupervisedChildRestarted
              , assertRestartCount (== 1)
              ]
            , andP
              [ assertEventType SupervisedChildRestarted
              , assertRestartCount (== 2)
              ]
            ]
          )
          ( \supervisor -> do
            childAction <- failingChild 2
            _childId    <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Transient }
              childAction
              supervisor
            threadDelay 100
          )
        ]
      , testGroup
        "with permanent strategy"
        [ testSupervisor
          "does restart on completion"
          ( assertInOrder
            [ assertEventType SupervisedChildStarted
            , assertEventType SupervisedChildCompleted
            , assertEventType SupervisedChildRestarted
            , assertEventType SupervisorTerminated
            ]
          )
          ( \supervisor -> do
            childAction <- completingChild 1 1
            _childId    <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Permanent }
              childAction
              supervisor
            threadDelay 100
          )
        , testSupervisor
          "does not increase restart count on multiple completions"
          ( assertInOrder
            [ andP
              [ assertEventType SupervisedChildRestarted
              , assertRestartCount (== 1)
              ]
            , andP
              [ assertEventType SupervisedChildRestarted
              , assertRestartCount (== 1)
              ]
            ]
          )
          ( \supervisor -> do
            childAction <- completingChild 2 1
            _childId    <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Permanent }
              childAction
              supervisor
            threadDelay 500
          )
        , testSupervisor
          "does restart on termination"
          ( assertInOrder
            [ assertEventType SupervisedChildTerminated
            , assertEventType SupervisedChildRestarted
            ]
          )
          ( \supervisor -> do
            childId <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Permanent }
              (forever $ threadDelay 10001000)
              supervisor
            SUT.terminateChild "testing termination (1)" childId supervisor
            threadDelay 100
          )
        , testSupervisor
          "does increase restart count on multiple terminations"
          ( assertInOrder
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
          )
          ( \supervisor -> do
            childId <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Permanent }
              (forever $ threadDelay 10001000)
              supervisor
            SUT.terminateChild "testing termination (1)" childId supervisor
            threadDelay 100
            SUT.terminateChild "testing termination (2)" childId supervisor
            threadDelay 100
          )
        , testSupervisor
          "does restart on failure"
          ( assertInOrder
            [ assertEventType SupervisedChildStarted
            , assertEventType SupervisedChildFailed
            , andP
              [ assertEventType SupervisedChildRestarted
              , assertRestartCount (== 1)
              ]
            ]
          )
          ( \supervisor -> do
            childAction <- failingChild 1
            _childId    <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Permanent }
              childAction
              supervisor
            threadDelay 100
          )
        , testSupervisor
          "does increase restart count on multiple failures"
          ( assertInOrder
            [ andP
              [ assertEventType SupervisedChildRestarted
              , assertRestartCount (== 1)
              ]
            , andP
              [ assertEventType SupervisedChildRestarted
              , assertRestartCount (== 2)
              ]
            ]
          )
          ( \supervisor -> do
            childAction <- failingChild 2
            _childId    <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Permanent }
              childAction
              supervisor
            threadDelay 100
          )
        ]
      , testGroup
        "with temporary strategy"
        [ testSupervisor
          "does not restart on completion"
          ( combineAssertions
            [ assertInOrder
              [ assertEventType SupervisedChildStarted
              , assertEventType SupervisedChildCompleted
              , assertEventType SupervisorTerminated
              ]
            , assertAll (not . assertEventType SupervisedChildRestarted)
            ]
          )
          ( \supervisor -> do
            _childId <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Temporary }
              (return ())
              supervisor
            threadDelay 10
          )
        , testSupervisor
          "does not restart on termination"
          ( combineAssertions
            [ assertInOrder
              [ assertEventType SupervisedChildStarted
              , assertEventType SupervisedChildTerminated
              , assertEventType SupervisorTerminated
              ]
            , assertAll (not . assertEventType SupervisedChildRestarted)
            ]
          )
          ( \supervisor -> do
            childId <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Temporary }
              (forever $ threadDelay 1000100)
              supervisor
            SUT.terminateChild "termination test (1)" childId supervisor
            threadDelay 100
          )
        , testSupervisor
          "does not restart on failure"
          ( combineAssertions
            [ assertInOrder
              [ assertEventType SupervisedChildStarted
              , assertEventType SupervisedChildFailed
              , assertEventType SupervisorTerminated
              ]
            , assertAll (not . assertEventType SupervisedChildRestarted)
            ]
          )
          ( \supervisor -> do
            _childId <- SUT.forkChild
              SUT.defChildOptions { SUT.childRestartStrategy = SUT.Temporary }
              (panic "child failed!")
              supervisor
            threadDelay 500
          )
        ]
      ]
    , testGroup
      "multiple supervised IO sub-routines"
      [ testSupervisor
        "terminates all supervised sub-routines on teardown"
        ( assertInOrder
          [ assertEventType SupervisedChildTerminated
          , assertEventType SupervisedChildTerminated
          , assertEventType SupervisorTerminated
          ]
        )
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

          threadDelay 500
        )
      , testGroup
        "with one for one supervisor restart strategy"
        [ testSupervisorWithOptions
            "restarts failing sub-routine only"
            ( combineAssertions
              [ assertInOrder
                [ andP
                    [ assertEventType SupervisedChildRestarted
                    , assertChildName "B"
                    ]
                ]
              , assertAll
                ( not
                . andP
                    [ assertEventType SupervisedChildRestarted
                    , assertChildName "A"
                    ]
                )
              ]
            )
            ( \supOptions ->
              supOptions { SUT.supervisorRestartStrategy = SUT.OneForOne }
            )
            ( \supervisor -> do
              _childA <- SUT.forkChild
                SUT.defChildOptions { SUT.childName            = "A"
                                    , SUT.childRestartStrategy = SUT.Temporary
                                    }
                (forever $ threadDelay 1000100)
                supervisor

              ioB     <- failingChild 1

              _childB <- SUT.forkChild
                SUT.defChildOptions { SUT.childName            = "B"
                                    , SUT.childRestartStrategy = SUT.Permanent
                                    }
                (forever $ ioB >> threadDelay 1000100)
                supervisor

              threadDelay 500
            )
        ]
      , testGroup
        "with all for one supervisor restart strategy with newest first order"
        [ testSupervisorWithOptions
          "does not restart sub-routines that are temporary"
          ( combineAssertions
            [ assertInOrder
              [ andP
                  [ assertEventType SupervisedChildRestarted
                  , assertChildName "A"
                  ]
              ]
            , assertAll
              ( not
              . andP
                  [ assertEventType SupervisedChildRestarted
                  , assertChildName "B"
                  ]
              )
            ]
          )
          ( \supOptions -> supOptions
            { SUT.supervisorRestartStrategy = SUT.AllForOne
            , SUT.supervisorChildTerminationOrder = SUT.OldestFirst
            }
          )
          ( \supervisor -> do
            ioA     <- failingChild 1
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

            threadDelay 500
          )
        , testSupervisorWithOptions
          "restarts sub-routines that are not temporary"
          ( assertInOrder
            [ andP
              [assertEventType SupervisedChildRestarted, assertChildName "B"]
            , andP
              [assertEventType SupervisedChildRestarted, assertChildName "A"]
            ]
          )
          ( \supOptions -> supOptions
            { SUT.supervisorRestartStrategy = SUT.AllForOne
            , SUT.supervisorChildTerminationOrder = SUT.NewestFirst
            }
          )
          ( \supervisor -> do
            ioA     <- failingChild 1
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

            threadDelay 500
          )
        ]
      , testGroup
        "with all for one supervisor restart strategy with oldest first order"
        [ testSupervisorWithOptions
          "does not restart sub-routines that are temporary"
          ( combineAssertions
            [ assertInOrder
              [ andP
                  [ assertEventType SupervisedChildRestarted
                  , assertChildName "A"
                  ]
              ]
            , assertAll
              ( not
              . andP
                  [ assertEventType SupervisedChildRestarted
                  , assertChildName "B"
                  ]
              )
            ]
          )
          ( \supOptions -> supOptions
            { SUT.supervisorRestartStrategy = SUT.AllForOne
            , SUT.supervisorChildTerminationOrder = SUT.OldestFirst
            }
          )
          ( \supervisor -> do
            ioA     <- failingChild 1
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

            threadDelay 500
          )
        , testSupervisorWithOptions
          "restarts sub-routines that are not temporary"
          ( assertInOrder
            [ andP
              [assertEventType SupervisedChildRestarted, assertChildName "A"]
            , andP
              [assertEventType SupervisedChildRestarted, assertChildName "B"]
            ]
          )
          ( \supOptions -> supOptions
            { SUT.supervisorRestartStrategy = SUT.AllForOne
            , SUT.supervisorChildTerminationOrder = SUT.OldestFirst
            }
          )
          ( \supervisor -> do
            ioA     <- failingChild 1
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

            threadDelay 500
          )
        ]
      ]
    ]

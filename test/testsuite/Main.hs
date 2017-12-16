{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import Protolude

import Data.IORef       (atomicModifyIORef', newIORef, readIORef)
import Text.Show.Pretty (ppShow)

import Test.Tasty                   (TestTree, defaultMainWithIngredients, testGroup)
import Test.Tasty.HUnit             (assertFailure, testCase)
import Test.Tasty.Ingredients.Rerun (rerunningTests)
import Test.Tasty.Runners           (consoleTestReporter, listingTests)

import qualified Data.Text as T

import qualified Control.Concurrent.Supervisor as SUT

main :: IO ()
main = defaultMainWithIngredients
  [rerunningTests [listingTests, consoleTestReporter]]
  (testGroup "supervisor" tests)

--------------------------------------------------------------------------------
-- Util

fetchEventName :: SUT.SupervisorEvent -> Text
fetchEventName = T.takeWhile (/= ' ') . show

andP :: [(a -> Bool)] -> a -> Bool
andP predList a = and $ map ($ a) predList

orP :: [(a -> Bool)] -> a -> Bool
orP predList a = or $ map ($ a) predList

--------------------------------------------------------------------------------
-- Assertions and Testers

combineAssertions :: [[SUT.SupervisorEvent]-> IO ()] -> [SUT.SupervisorEvent] -> IO ()
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
          <> (show $ length assertions')
          <> " assertions remaining, "
          <> (show $ length events0)
          <> " events tried)\n"
          <> (ppShow $ zip ([0 ..] :: [Int]) events0)
          )
  in  loop assertions0 events0

assertAll :: (SUT.SupervisorEvent -> Bool) -> [SUT.SupervisorEvent] -> IO ()
assertAll predFn events =
  if all predFn events then
    return ()
  else
    assertFailure
          (  "Expected all events to match predicate, but didn't ("
          <> (show $ length events)
          <> " events tried)\n"
          <> (ppShow $ zip ([0 ..] :: [Int]) events)
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
assertEventType evType ev = fetchEventName ev == show evType

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

restartingChild :: Int -> IO (IO ())
restartingChild failCount = do
  countRef <- newIORef failCount
  return $ do
    shouldFail <- atomicModifyIORef' countRef
                    (\count ->
                        (pred count, count > 0))
    when shouldFail $ do
      throwIO RestartingChildError

testSupervisorWithOptions
  :: [Char]
  -> ([SUT.SupervisorEvent] -> IO ())
  -> (SUT.SupervisorOptions -> SUT.SupervisorOptions)
  -> (SUT.Supervisor -> IO ())
  -> TestTree
testSupervisorWithOptions testCaseStr assertionsFn optionModFn setupFn =
  testCase testCaseStr $ do
    accRef     <- newIORef []
    supervisor <- SUT.forkSupervisor $ (optionModFn $ SUT.defSupervisorOptions)
      { SUT.notifyEvent = trackEvent accRef
      }
    setupFn supervisor `finally` SUT.teardown supervisor
    events <- readIORef accRef
    assertionsFn (reverse events)
 where
  trackEvent accRef ev = atomicModifyIORef' accRef (\old -> (ev : old, ()))

testSupervisor
  :: [Char]
  -> ([SUT.SupervisorEvent] -> IO ())
  -> (SUT.Supervisor -> IO ())
  -> TestTree
testSupervisor testCaseStr assertionsFn setupFn =
  testSupervisorWithOptions testCaseStr assertionsFn identity setupFn

--------------------------------------------------------------------------------
-- Actual Tests

tests :: [TestTree]
tests =
  [ testSupervisor
      "initialize and teardown without supervised action"
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

  , testGroup "single supervised IO sub-routine"
    [
      testGroup "with transient strategy"
      [
        testSupervisor "does not execute restart on completion"
          (combineAssertions
          [
            assertInOrder
            [ assertEventType SupervisedChildStarted
            , assertEventType SupervisedChildCompleted
            , assertEventType SupervisorTerminated
            ]
          , assertAll
            ( not . assertEventType SupervisedChildRestarted )
          ]
          )
          (\supervisor -> do
              _childId <- SUT.forkChild
                            SUT.defChildOptions { SUT.childRestartStrategy = SUT.Transient }
                            (return ())
                            supervisor
              threadDelay 1000
          )
      , testSupervisor "does execute restart on failure"
        (assertInOrder
          [ assertEventType SupervisedChildStarted
          , assertEventType SupervisedChildFailed
          , assertEventType SupervisedChildRestarted
          ]
        )
        (\supervisor -> do
              childAction <- restartingChild 1
              _childId <- SUT.forkChild
                            SUT.defChildOptions { SUT.childRestartStrategy = SUT.Transient }
                            childAction
                            supervisor
              threadDelay 1000
            )
      ]
    ]
  ]

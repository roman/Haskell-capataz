{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Data.IORef (atomicModifyIORef', newIORef, readIORef)
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

assertInOrder :: [SUT.SupervisorEvent -> Bool] -> [SUT.SupervisorEvent] -> IO ()
assertInOrder assertions0 events0 =
  let loop assertions events =
        case (assertions, events) of
          ([], _) ->
            return ()
          (assertionFn:assertions', event:events')
            | assertionFn event -> loop assertions' events'
            | otherwise -> loop assertions events'
          (assertions', []) ->
            assertFailure ("Expected all assertions to match, but didn't ("
                           <> (show $ length assertions')
                           <> " assertions remaining, "
                           <> (show $ length events0)
                           <> " events tried)\n"
                           <> (ppShow $ zip ([0..] :: [Int]) events0))
  in loop assertions0 events0

assertEventType :: Text -> SUT.SupervisorEvent -> Bool
assertEventType evName ev =
  fetchEventName ev == evName

assertSupervisorStatusChanged
  :: SUT.SupervisorStatus
  -> SUT.SupervisorStatus
  -> SUT.SupervisorEvent
  -> Bool
assertSupervisorStatusChanged fromEv toEv ev =
  case ev of
    SUT.SupervisorStatusChanged {prevSupervisorStatus, newSupervisorStatus} ->
      fromEv == prevSupervisorStatus && toEv == newSupervisorStatus
    _ ->
      False

testSupervisor
  :: ([SUT.SupervisorEvent] -> IO ())
  -> (SUT.SupervisorSpec -> IO ())
  -> IO ()
testSupervisor assertionsFn callbackFn = do
  accRef <- newIORef []
  callbackFn (SUT.defSupervisorSpec { SUT.notifyEvent = trackEvent accRef })
  events <- readIORef accRef
  assertionsFn (reverse events)
 where
  trackEvent accRef ev = atomicModifyIORef' accRef (\old -> (ev : old, ()))

--------------------------------------------------------------------------------
-- Actual Tests

tests :: [TestTree]
tests =
  [ testCase "initialize and teardown without children" $ do
      testSupervisor
        (assertInOrder [ andP [ assertEventType "SupervisorStatusChanged"
                              , assertSupervisorStatusChanged SUT.Initializing SUT.Running
                              ]
                       , andP [ assertEventType "SupervisorStatusChanged"
                              , assertSupervisorStatusChanged SUT.Running SUT.Halted
                              ]
                       , assertEventType "SupervisorTerminated"
                       ])
        (\supSpec -> do
          supervisor <- SUT.forkSupervisor supSpec
          threadDelay 500
          void $ SUT.teardown supervisor)

  -- testGroup "Create children"
  ]
-- tests = [
--   testGroup "SmallCheck" scTests, testGroup "Unit tests" huTests
--   ]

-- scTests :: [TestTree]
-- scTests =
--   [ testProperty "inc == succ"                   prop_succ
--   , testProperty "inc . negate == negate . pred" prop_pred
--   ]

-- huTests :: [TestTree]
-- huTests =
--   [ testCase "Increment below TheAnswer" case_inc_below
--   , testCase "Decrement above TheAnswer" case_dec_above
--   ]

-- prop_succ :: Int -> Bool
-- prop_succ n = inc n == succ n

-- prop_pred :: Int -> Bool
-- prop_pred n = inc (negate n) == negate (pred n)

-- case_inc_below :: Assertion
-- case_inc_below = inc 41 @?= (42 :: Int)

-- case_dec_above :: Assertion
-- case_dec_above = negate (inc (negate 43)) @?= (42 :: Int)

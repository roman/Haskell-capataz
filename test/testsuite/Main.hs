{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Text.Show.Pretty (pPrint)

import Test.Tasty                   (TestTree, defaultMainWithIngredients, testGroup)
-- import Test.Tasty.HUnit             (assertEqual, testCase)
import Test.Tasty.HUnit             (testCase)
import Test.Tasty.Ingredients.Rerun (rerunningTests)
import Test.Tasty.Runners           (consoleTestReporter, listingTests)
-- import Test.Tasty.SmallCheck        (testProperty)

import qualified Control.Concurrent.Supervisor as SUT

main :: IO ()
main = defaultMainWithIngredients
  [rerunningTests [listingTests, consoleTestReporter]]
  (testGroup "supervisor" tests)

assertSupervisor
  :: ([SUT.SupervisorEvent] -> IO ()) -> (SUT.SupervisorSpec -> IO ()) -> IO ()
assertSupervisor assertionsFn callbackFn = do
  accRef <- newIORef []
  callbackFn (SUT.defSupervisorSpec { SUT.notifyEvent = trackEvent accRef })
  events <- readIORef accRef
  assertionsFn (reverse events)
 where
  trackEvent accRef ev = atomicModifyIORef' accRef (\old -> (ev : old, ()))


tests :: [TestTree]
tests =
  [ testCase "initialize and teardown supervisor" $ do
      assertSupervisor pPrint $ \supSpec -> do
        supervisor <- SUT.forkSupervisor supSpec
        _childId   <- SUT.forkChild SUT.defChildOptions
                                    (forever $ threadDelay 1000100)
                                    supervisor
        threadDelay 3000
        void $ SUT.teardown supervisor

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

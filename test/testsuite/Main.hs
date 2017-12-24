{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main where

import Protolude

import Control.Concurrent.SupervisorTest
import Test.Tasty                        (defaultMainWithIngredients, testGroup)
import Test.Tasty.Ingredients.Rerun      (rerunningTests)
import Test.Tasty.Runners                (consoleTestReporter, listingTests)

main :: IO ()
main = defaultMainWithIngredients
  [rerunningTests [listingTests, consoleTestReporter]]
  (testGroup "supervisor" tests)

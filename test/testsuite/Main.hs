{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main where

import RIO

import qualified Control.Concurrent.Capataz.SupervisorTest as Supervisor
import qualified Control.Concurrent.CapatazTest            as Capataz
import           Test.Tasty
    (defaultMainWithIngredients, testGroup)
import           Test.Tasty.Runners
    (consoleTestReporter, listingTests)

main :: IO ()
main = defaultMainWithIngredients
  [listingTests, consoleTestReporter]
  (testGroup "capataz" [Capataz.tests, Supervisor.tests])

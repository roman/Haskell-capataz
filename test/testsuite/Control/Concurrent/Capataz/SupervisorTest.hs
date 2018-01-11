{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Concurrent.Capataz.SupervisorTest where

import Protolude

import Test.Util
import qualified Control.Concurrent.Capataz as SUT

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup "supervision tree"
  [
    testCase "sub-tree gets restarted on failure" $ do
      failingAction <- mkFailingSubRoutine 3
      testCapatazStreamWithOptions
        (\ supOptions -> supOptions
          {
            SUT.supervisorIntensity = 3
          , SUT.supervisorProcessSpecList =
            [
              SUT.SupervisorProcessSpec SUT.defSupervisorSpec
              {
                SUT.supervisorName = "tree-1"
              , SUT.supervisorIntensity = 1
              , SUT.supervisorProcessSpecList =
                [
                  SUT.WorkerProcessSpec SUT.defWorkerSpec
                  {
                    SUT.workerName = "failing-worker"
                  , SUT.workerAction = failingAction
                  }
                ]
              }
            ]
          }
        )
        []
        (const $ threadDelay 100100)
        [ assertWorkerFailed "failing-worker"
        , assertSupervisorFailed "tree-1"
        , assertSupervisorRestarted "tree-1"
        , assertWorkerStarted "failing-worker" ]
        []
        Nothing


  ]

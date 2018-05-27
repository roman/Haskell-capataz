{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Control.Concurrent.Capataz.SupervisorTest where

import RIO

import           Control.Concurrent.Capataz (set)
import qualified Control.Concurrent.Capataz as SUT
import           Test.Util

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests = testGroup
  "supervision trees"
  [ testCase "initialize and teardown of supervision tree works as expected"
    $ testCapatazStreamWithOptions
        (& set
          SUT.supervisorProcessSpecListL
          [ SUT.supervisorSpec
            "tree-1"
            (set
              SUT.supervisorProcessSpecListL
              [ SUT.workerSpecWithDefaults "1-A"
                                           (forever $ threadDelay 10001000)
              , SUT.workerSpecWithDefaults "1-B"
                                           (forever $ threadDelay 10001000)
              ]
            )
          , SUT.supervisorSpec
            "tree-2"
            (set
              SUT.supervisorProcessSpecListL
              [ SUT.workerSpecWithDefaults "2-A"
                                           (forever $ threadDelay 10001000)
              , SUT.workerSpecWithDefaults "2-B"
                                           (forever $ threadDelay 10001000)
              ]
            )
          ]
        )
        [ andP [assertSupervisorName "tree-1", assertWorkerStarted "1-A"]
        , andP [assertSupervisorName "tree-1", assertWorkerStarted "1-B"]
        , andP
          [ assertSupervisorName "tree-1"
          , assertEventType SupervisorStatusChanged
          , assertSupervisorStatusChanged SUT.Initializing SUT.Running
          ]
        , andP [assertSupervisorName "tree-2", assertWorkerStarted "2-A"]
        , andP [assertSupervisorName "tree-2", assertWorkerStarted "2-B"]
        , andP
          [ assertSupervisorName "tree-2"
          , assertEventType SupervisorStatusChanged
          , assertSupervisorStatusChanged SUT.Initializing SUT.Running
          ]
        , andP
          [ assertRootSupervisor
          , assertEventType SupervisorStatusChanged
          , assertSupervisorStatusChanged SUT.Initializing SUT.Running
          ]
        ]
        (const $ return ())
        []
        [ andP
          [ assertRootSupervisor
          , assertEventType SupervisorStatusChanged
          , assertSupervisorStatusChanged SUT.Running SUT.Halting
          ]
        , andP [assertRootSupervisor, assertEventType ProcessTerminationStarted]
        , andP [assertSupervisorName "tree-1", assertWorkerTerminated "1-A"]
        , andP [assertSupervisorName "tree-1", assertWorkerTerminated "1-B"]
        , andP [assertSupervisorName "tree-2", assertWorkerTerminated "2-A"]
        , andP [assertSupervisorName "tree-2", assertWorkerTerminated "2-B"]
        , andP
          [assertRootSupervisor, assertEventType ProcessTerminationFinished]
        , andP
          [ assertRootSupervisor
          , assertEventType SupervisorStatusChanged
          , assertSupervisorStatusChanged SUT.Halting SUT.Halted
          ]
        ]
        Nothing
  , testCase "supervision sub-tree gets restarted on failure" $ do
    failingAction <- mkFailingSubRoutine 2
    testCapatazStreamWithOptions
      (set SUT.supervisorIntensityL 3 . set
        SUT.supervisorProcessSpecListL
        [ SUT.supervisorSpec
            "tree-1"
            ( set SUT.supervisorIntensityL     1
            . set SUT.supervisorPeriodSecondsL 10
            . set
                SUT.supervisorProcessSpecListL
                [SUT.workerSpecWithDefaults "failing-worker" failingAction]
            )
        ]
      )
      []
      (const $ threadDelay 1000)
      [ assertWorkerFailed "failing-worker"
      , assertSupervisorFailed "tree-1"
      , assertWorkerStarted "failing-worker"
      , assertSupervisorRestarted "tree-1"
      ]
      []
      Nothing
  , testCase "AllForOne strategy restarts sibling supervision tree" $ do
    failingAction <- mkFailingSubRoutine 2
    testCapatazStreamWithOptions
      ( set SUT.supervisorIntensityL       3
      . set SUT.supervisorRestartStrategyL SUT.AllForOne
      . set
          SUT.supervisorProcessSpecListL
          [ SUT.supervisorSpec
            "tree-1"
            ( set SUT.supervisorIntensityL     1
            . set SUT.supervisorPeriodSecondsL 10
            . set
                SUT.supervisorProcessSpecListL
                [SUT.workerSpecWithDefaults "failing-worker" failingAction]
            )
          , SUT.supervisorSpec
            "tree-2"
            (set
              SUT.supervisorProcessSpecListL
              [ SUT.workerSpec "stable-worker"
                               (forever $ threadDelay 1000100)
                               id
              ]
            )
          ]
      )
      []
      (const $ threadDelay 9000)
      [ assertWorkerFailed "failing-worker"
      , assertWorkerStarted "stable-worker"
      , assertSupervisorFailed "tree-1"
      , assertSupervisorRestarted "tree-1"
      , assertSupervisorTerminated "tree-2"
      , assertWorkerStarted "stable-worker"
      , assertSupervisorRestarted "tree-2"
      ]
      []
      Nothing
  ]

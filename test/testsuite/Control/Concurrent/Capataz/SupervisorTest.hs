{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Control.Concurrent.Capataz.SupervisorTest where

import Protolude

import qualified Control.Concurrent.Capataz as SUT
import           Test.Util

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests = testGroup
  "supervision trees"
  [ testCase "initialize and teardown of supervision tree works as expected"
    $ testCapatazStreamWithOptions
        ( \supOptions -> supOptions
          { SUT.supervisorProcessSpecList = [ SUT.SupervisorSpec
                                              $ SUT.defSupervisorOptions
                                                  { SUT.supervisorName = "tree-1"
                                                  , SUT.supervisorProcessSpecList = [ SUT.WorkerSpec
                                                                                      $ SUT.defWorkerOptions
                                                                                          { SUT.workerName = "1-A"
                                                                                          , SUT.workerAction = forever
                                                                                            ( threadDelay
                                                                                              10001000
                                                                                            )
                                                                                          }
                                                                                    , SUT.WorkerSpec
                                                                                      $ SUT.defWorkerOptions
                                                                                          { SUT.workerName = "1-B"
                                                                                          , SUT.workerAction = forever
                                                                                            ( threadDelay
                                                                                              10001000
                                                                                            )
                                                                                          }
                                                                                    ]
                                                  }
                                            , SUT.SupervisorSpec
                                              $ SUT.defSupervisorOptions
                                                  { SUT.supervisorName = "tree-2"
                                                  , SUT.supervisorProcessSpecList = [ SUT.WorkerSpec
                                                                                      $ SUT.defWorkerOptions
                                                                                          { SUT.workerName = "2-A"
                                                                                          , SUT.workerAction = forever
                                                                                            ( threadDelay
                                                                                              10001000
                                                                                            )
                                                                                          }
                                                                                    , SUT.WorkerSpec
                                                                                      $ SUT.defWorkerOptions
                                                                                          { SUT.workerName = "2-B"
                                                                                          , SUT.workerAction = forever
                                                                                            ( threadDelay
                                                                                              10001000
                                                                                            )
                                                                                          }
                                                                                    ]
                                                  }
                                            ]
          }
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
          [ assertSupervisorName "capataz-root"
          , assertEventType SupervisorStatusChanged
          , assertSupervisorStatusChanged SUT.Initializing SUT.Running
          ]
        ]
        (const $ return ())
        []
        [ andP
          [ assertSupervisorName "capataz-root"
          , assertEventType SupervisorStatusChanged
          , assertSupervisorStatusChanged SUT.Running SUT.Halting
          ]
        , andP
          [ assertSupervisorName "capataz-root"
          , assertEventType ProcessTerminationStarted
          ]
        , andP [assertSupervisorName "tree-1", assertWorkerTerminated "1-A"]
        , andP [assertSupervisorName "tree-1", assertWorkerTerminated "1-B"]
        , andP [assertSupervisorName "tree-2", assertWorkerTerminated "2-A"]
        , andP [assertSupervisorName "tree-2", assertWorkerTerminated "2-B"]
        , andP
          [ assertSupervisorName "capataz-root"
          , assertEventType ProcessTerminationFinished
          ]
        , andP
          [ assertSupervisorName "capataz-root"
          , assertEventType SupervisorStatusChanged
          , assertSupervisorStatusChanged SUT.Halting SUT.Halted
          ]
        ]
        Nothing
  , testCase "supervision sub-tree gets restarted on failure" $ do
    failingAction <- mkFailingSubRoutine 2
    testCapatazStreamWithOptions
      ( \supOptions -> supOptions
        { SUT.supervisorIntensity       = 3
        , SUT.supervisorProcessSpecList = [ SUT.SupervisorSpec
                                              SUT.defSupervisorOptions
                                                { SUT.supervisorName = "tree-1"
                                                , SUT.supervisorIntensity = 1
                                                , SUT.supervisorPeriodSeconds = 10
                                                , SUT.supervisorProcessSpecList = [ SUT.WorkerSpec
                                                                                      SUT.defWorkerOptions
                                                                                        { SUT.workerName = "failing-worker"
                                                                                        , SUT.workerAction = failingAction
                                                                                        }
                                                                                  ]
                                                }
                                          ]
        }
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
      ( \supOptions -> supOptions
        { SUT.supervisorIntensity       = 3
        , SUT.supervisorRestartStrategy = SUT.AllForOne
        , SUT.supervisorProcessSpecList = [ SUT.SupervisorSpec
                                            SUT.defSupervisorOptions
                                              { SUT.supervisorName = "tree-1"
                                              , SUT.supervisorIntensity = 1
                                              , SUT.supervisorPeriodSeconds = 10
                                              , SUT.supervisorProcessSpecList = [ SUT.WorkerSpec
                                                                                    SUT.defWorkerOptions
                                                                                      { SUT.workerName = "failing-worker"
                                                                                      , SUT.workerAction = failingAction
                                                                                      }
                                                                                ]
                                              }
                                          , SUT.SupervisorSpec
                                            SUT.defSupervisorOptions
                                              { SUT.supervisorName = "tree-2"
                                              , SUT.supervisorProcessSpecList = [ SUT.WorkerSpec
                                                                                    SUT.defWorkerOptions
                                                                                      { SUT.workerName = "stable-worker"
                                                                                      , SUT.workerAction = forever
                                                                                        $ threadDelay
                                                                                            1000100
                                                                                      }
                                                                                ]
                                              }
                                          ]
        }
      )
      []
      (const $ threadDelay 1000)
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

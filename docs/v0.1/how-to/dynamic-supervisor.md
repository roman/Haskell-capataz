# Create a dynamic supervisor

## Description

The following example:

* creates a capataz system
* forks a supervisor process dynamically
* forks a [worker process dynamically](./dynamic-worker.md)
* tears down the capataz system and show results of teardown operation

## Code

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Capataz.HowTo.DynSupervisor where

import Control.Concurrent.Capataz
  (
    forkCapataz
  , forkSupervisor
  , buildSupervisorOptions
  , supervisorRestartStrategyL
  , SupervisorRestartStrategy(..)
  , forkWorker
  , buildWorkerOptions
  , workerRestartStrategyL
  , WorkerRestartStrategy(..)
  , teardown
  , set
  )

myUselessWorker :: IO ()
myUselessWorker = return ()

main :: IO ()
main = do
  capataz <- forkCapataz "dynamic-supervisor-example"
                         (set supervisorRestartStrategyL OneForOne)

  supervisor <-
    forkSupervisor
    ( buildSupervisorOptions "unnecessary-supervisor"
                             (set supervisorRestartStrategyL AllForOne)
    )
    capataz

  _workerId  <-
    forkWorker
      ( buildWorkerOptions "useless-worker"
                           myUselessWorker
                           (set workerRestartStrategyL Transient)
      )
      supervisor

  result <- teardown capataz
  print result
```

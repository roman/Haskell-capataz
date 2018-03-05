# Terminate a Supervisor

**NOTE**: Only supervisors created dynamically can be terminated

## Description

The following example:

* creates a capataz system
* forks a supervisor process dynamically
* forks a worker process dynamically
* terminates supervisor after a delay
* tears down the capataz system and show results of teardown operation

## Code

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Control.Concurrent.Capataz.HowTo.TerminateSupervisor where

import Control.Concurrent (threadDelay)
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
  , getSupervisorProcessId
  , terminateProcess
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

  threadDelay 3000100

  supervisorTerminated <-
      terminateProcess "reason: terminate supervisor demo" (getSupervisorProcessId supervisor) capataz

  result <- teardown capataz
  putStrLn $ "Supervisor terminated? " ++ show supervisorTerminated
  print result
```

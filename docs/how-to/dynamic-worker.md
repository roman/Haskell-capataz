# Create a dynamic worker

## Description

The following example:

* creates a capataz system
* forks a worker process dynamically
* tears down the capataz system and show results of teardown operation

## Code

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Capataz.HowTo.DynWorker where

import Control.Concurrent.Capataz
  (
    forkCapataz
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
  capataz <- forkCapataz "dynamic-worker-example" id
  _workerId  <-
    forkWorker
      ( buildWorkerOptions "useless-worker"
                           myUselessWorker
                           (set workerRestartStrategyL Transient)
      )
      capataz
  result <- teardown capataz
  print result
```

# Terminate a worker

*NOTE*: Only workers created dynamically can be terminated

## Description

The following example:

* creates a capataz system
* forks a worker process dynamically
* terminates the worker after a delay
* tears down the capataz system and show results of teardown operation

## Code

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Control.Concurrent.Capataz.HowTo.TerminateWorker where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Concurrent.Capataz
  (
    forkCapataz
  , forkWorker
  , buildWorkerOptions
  , workerRestartStrategyL
  , WorkerRestartStrategy(..)
  , terminateProcess
  , teardown
  , set
  )

infiniteContent :: IO ()
infiniteContent = forever $ threadDelay 100010

main :: IO ()
main = do
  capataz <- forkCapataz "dynamic-worker-example" id
  workerId  <-
    forkWorker
      ( buildWorkerOptions "infinite-content"
                           infiniteContent
                           (set workerRestartStrategyL Transient)
      )
      capataz

  threadDelay 5000100 -- 5 seconds
  didTerminate <- terminateProcess "reason: terminate for demo" workerId capataz

  result <- teardown capataz
  putStrLn $ "Worker terminated? " ++ show didTerminate
  print result
```

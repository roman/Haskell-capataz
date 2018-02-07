# Create a static

## Description

The following example:

* creates a worker specification
* creates a capataz system and a worker (using worker specification)
* tears down the capataz system and show results of teardown operation

## Code

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Capataz.HowTo.StaticWorker where

import Control.Concurrent.Capataz
  (
    forkCapataz
  , supervisorProcessSpecListL
  , workerSpec
  , workerRestartStrategyL
  , WorkerRestartStrategy(..)
  , teardown
  , set
  )

myUselessWorker :: IO ()
myUselessWorker = return ()

main :: IO ()
main = do
  let
    myWorker =
      workerSpec "useless-worker" myUselessWorker (set workerRestartStrategyL Transient)

  capataz <-
    forkCapataz "static-worker-example"
       ( set supervisorProcessSpecListL [myWorker] )

  result <- teardown capataz
  print result
```

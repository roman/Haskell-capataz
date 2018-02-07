# Create a static supervisor

## Description

The following example:

* creates a worker specification
* creates a supervisor specification with worker specification embedded
* creates a capataz system and an inner supervisor (using specification)
* tears down the capataz system and show results of teardown operation

## Code

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Capataz.HowTo.StaticSupervisor where

import Control.Concurrent.Capataz
  (
    forkCapataz
  , supervisorProcessSpecListL
  , supervisorRestartStrategyL
  , SupervisorRestartStrategy(..)
  , supervisorSpec
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
      workerSpec "useless-worker"
                  myUselessWorker
                  (set workerRestartStrategyL Transient)

    mySupervisor =
      supervisorSpec "unnecessary-supervisor"
        ( set supervisorRestartStrategyL AllForOne
        . set supervisorProcessSpecListL [myWorker])


  capataz <- forkCapataz "dynamic-supervisor-example"
                         ( set supervisorRestartStrategyL OneForOne
                         . set supervisorProcessSpecListL [mySupervisor]
                         )

  result <- teardown capataz
  print result
```

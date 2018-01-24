{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Capataz
    ( SupervisorRestartStrategy (..)
    , WorkerRestartStrategy (..)
    , buildCapatazOptions
    , buildWorkerOptions
    , forkCapataz
    , forkWorker
    , getSupervisorAsync
    , notifyEventL
    , set
    , supervisorNameL
    , supervisorRestartStrategyL
    , teardown
    , workerRestartStrategyL
    )
import Lib                        (Cli (..), killNumberProcess, spawnNumbersProcess)
import Options.Generic            (getRecord)
import Protolude
import Text.Show.Pretty           (pPrint)


main :: IO ()
main = do
  n <- getRecord "Counter spawner"

  let myCapatazOptions = buildCapatazOptions
        ( set supervisorNameL            "Example Capataz"
        . set supervisorRestartStrategyL OneForOne
        . set notifyEventL               pPrint
        )

  capataz <- forkCapataz myCapatazOptions

  let numberWriter i a = print (i, a)
      delayMicros = 5000100

  _workerIdList <- forM [1 .. procNumber n] $ \i -> forkWorker
    ( buildWorkerOptions ("Worker (" <> show i <> ")")
                         (spawnNumbersProcess (numberWriter i))
                         (set workerRestartStrategyL Permanent)
    )
    capataz

  let workerKillerOptions = buildWorkerOptions
        "worker-killer"
        (forever $ threadDelay delayMicros >> killNumberProcess)
        identity
  void $ forkWorker workerKillerOptions capataz

  wait (getSupervisorAsync capataz) `finally` (teardown capataz >>= print)

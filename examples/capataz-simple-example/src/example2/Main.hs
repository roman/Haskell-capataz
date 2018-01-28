{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Capataz
    ( SupervisorRestartStrategy (..)
    , WorkerRestartStrategy (..)
    , buildWorkerOptions
    , buildWorkerOptionsWithDefaults
    , joinCapatazThread
    , forkCapataz
    , forkWorker
    , onSystemEventL
    , set
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

  capataz <- forkCapataz ( set supervisorRestartStrategyL OneForOne
                         . set onSystemEventL pPrint
                         )

  let numberWriter i a = print (i, a)
      delayMicros = 5000100

  _workerIdList <- forM [1 .. procNumber n] $ \i -> forkWorker
    ( buildWorkerOptions ("Worker (" <> show i <> ")")
                         (spawnNumbersProcess (numberWriter i))
                         (set workerRestartStrategyL Permanent)
    )
    capataz

  let workerKillerOptions = buildWorkerOptionsWithDefaults
        "worker-killer"
        (forever $ threadDelay delayMicros >> killNumberProcess)

  void $ forkWorker workerKillerOptions capataz

  joinCapatazThread capataz `finally` (teardown capataz >>= print)

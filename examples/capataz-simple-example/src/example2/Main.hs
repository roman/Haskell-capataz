{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Prelude
import RIO

import Control.Concurrent.Capataz
    ( SupervisorRestartStrategy (..)
    , WorkerRestartStrategy (..)
    , buildWorkerOptions
    , buildWorkerOptionsWithDefaults
    , forkCapataz
    , forkWorker
    , joinCapatazThread
    , onSystemEventL
    , set
    , supervisorRestartStrategyL
    , runTeardown
    , workerRestartStrategyL
    )
import Lib                        (Cli (..), killNumberProcess, spawnNumbersProcess)
import Options.Generic            (getRecord)
import Text.Show.Pretty           (pPrint)

main :: IO ()
main = do
  n       <- getRecord "Counter spawner"

  capataz <- forkCapataz
    "unix-process-capataz" -- (1)
    (set supervisorRestartStrategyL OneForOne -- (2)
                                              . set onSystemEventL pPrint)                -- (3)

  let numberWriter i a = Prelude.print (i, a)
      delayMicros = 5000100

  _workerIdList <- forM [1 .. procNumber n] $ \i -> do
    let counterWorkerOptions = buildWorkerOptions -- (4)
          ("Worker (" <> tshow i <> ")")
          (spawnNumbersProcess (numberWriter i)) -- (5)
          (set workerRestartStrategyL Permanent) -- (6)

    forkWorker -- (7)
               counterWorkerOptions capataz

  let workerKillerOptions = buildWorkerOptionsWithDefaults -- (8)
        "worker-killer"
        (forever $ threadDelay delayMicros >> killNumberProcess)

  -- ignore returned ProcessId, as we won't use it in our example
  void $ forkWorker workerKillerOptions capataz

  joinCapatazThread capataz  -- (9)
                            `finally` (runTeardown capataz >>= Prelude.print) -- (10)

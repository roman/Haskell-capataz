{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Capataz
    ( SupervisorRestartStrategy (..)
    , WorkerRestartStrategy (..)
    , getSupervisorAsync
    , defCapatazOptions
    , defWorkerOptions
    , (.~)
    , supervisorNameL
    , supervisorRestartStrategyL
    , notifyEventL
    , workerRestartStrategyL
    , forkCapataz
    , forkWorker
    , teardown
    )
import Lib                        (Cli (..), killNumberProcess, spawnNumbersProcess)
import Options.Generic            (getRecord)
import Protolude
import Text.Show.Pretty           (pPrint)


main :: IO ()
main = do
  n       <- getRecord "Counter spawner"
  capataz <- forkCapataz (defCapatazOptions
                          & supervisorNameL .~ "Example Capataz"
                          & supervisorRestartStrategyL .~ OneForOne
                          & notifyEventL .~ pPrint)

  let numberWriter i a = print (i, a)
      delayMicros = 5000100

  _workerIdList <- forM [1 .. procNumber n] $ \i -> forkWorker
    (defWorkerOptions & workerRestartStrategyL .~ Permanent)
    ("Worker (" <> show i <> ")")
    (spawnNumbersProcess (numberWriter i))
    capataz

  void $ forkWorker defWorkerOptions
                    "worker-killer"
                    (forever $ threadDelay delayMicros >> killNumberProcess)
                    capataz

  wait (getSupervisorAsync capataz) `finally` (teardown capataz >>= print)

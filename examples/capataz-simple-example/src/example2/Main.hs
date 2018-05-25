{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

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
import Text.Show.Pretty           (ppShow)

main :: IO ()
main = do
  n       <- getRecord "Counter spawner"
  logOptions <- logOptionsHandle stdout True
  withLogFunc logOptions $ \logFunc -> runRIO logFunc $ do

    capataz <- forkCapataz
      "unix-process-capataz"
      (set supervisorRestartStrategyL OneForOne
       . set onSystemEventL (logDebug . displayShow . ppShow))

    let numberWriter i a = logInfo $ displayShow (i :: Int, a :: Int)
        delayMicros = 5000100

    _workerIdList <- forM [1 .. procNumber n] $ \i -> do
      let counterWorkerOptions = buildWorkerOptions
            ("Worker (" <> tshow i <> ")")
            (spawnNumbersProcess (numberWriter i))
            (set workerRestartStrategyL Permanent)

      forkWorker counterWorkerOptions capataz

    let workerKillerOptions = buildWorkerOptionsWithDefaults
          "worker-killer"
          (forever $ threadDelay delayMicros >> killNumberProcess)

    -- ignore returned ProcessId, as we won't use it in our example
    void $ forkWorker workerKillerOptions capataz

    finally (joinCapatazThread capataz)
            (liftIO (runTeardown capataz) >>= logDebug . displayShow)

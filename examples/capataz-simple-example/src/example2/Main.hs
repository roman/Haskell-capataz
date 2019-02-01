{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RIO

import Capataz
    ( CapatazOptions
    , SupervisorRestartStrategy (..)
    , WorkerRestartStrategy (..)
    , buildLogWorkerOptions1
    , buildWorkerOptions
    , buildWorkerOptionsWithDefaults
    , forkCapataz
    , forkWorker
    , joinCapatazThread
    , onSystemEventL
    , set
    , supervisorRestartStrategyL
    , terminateCapataz_
    , workerRestartStrategyL
    )
import Lib             (Cli (..), killNumberProcess, spawnNumbersProcess)
import Options.Generic (getRecord)

rootSupervisorOptions
  :: (HasLogFunc env, MonadIO m, MonadReader env m)
  => (CapatazOptions m -> CapatazOptions m)
rootSupervisorOptions = set supervisorRestartStrategyL OneForOne
  . set onSystemEventL (logDebug . display) -- Show all events of the capataz sub-system on debug


main :: IO ()
main = do
  n                        <- procNumber <$> getRecord "Counter spawner"
  logOptions               <- logOptionsHandle stdout False
  (loggerOptions, logFunc) <- buildLogWorkerOptions1 logOptions "logger" n id

  runRIO logFunc $ do
    capataz       <- forkCapataz "unix-process-capataz" rootSupervisorOptions

    _loggerWorker <- forkWorker loggerOptions capataz
    let numberWriter i a = logInfo $ displayShow (i :: Natural, a :: Natural)
        delayMicros = 5000100

    _workerIdList <- forM [1 .. n] $ \i -> do
      let counterWorkerOptions = buildWorkerOptions
            ("worker-" <> tshow i)
            (spawnNumbersProcess (numberWriter i))
            (set workerRestartStrategyL Permanent)

      forkWorker counterWorkerOptions capataz

    let workerKillerOptions = buildWorkerOptionsWithDefaults
          "worker-killer"
          (forever $ threadDelay delayMicros >> killNumberProcess)

    -- ignore returned ProcessId, as we won't use it in our example
    void $ forkWorker workerKillerOptions capataz

    finally (joinCapatazThread capataz)
            (terminateCapataz_ capataz >>= logDebug . displayShow)

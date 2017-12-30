{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Protolude
import Options.Generic (getRecord)
import Control.Concurrent.Capataz
  ( WorkerOptions(..)
  , CapatazOptions(..)
  , WorkerRestartStrategy(..)
  , CapatazRestartStrategy(..)
  , forkCapataz
  , forkWorker
  , defWorkerOptions
  , defCapatazOptions
  , capatazToAsync
  , teardown
  )
import Lib (Cli(..), spawnNumbersProcess, killNumberProcess)
import Text.Show.Pretty (pPrint)


main :: IO ()
main = do
  n <- getRecord "Counter spawner"
  capataz <-
    forkCapataz defCapatazOptions { capatazName = "Example Capataz"
                                        , capatazRestartStrategy = OneForOne
                                        , notifyEvent = pPrint }

  let numberWriter i a = print (i, a)
      delayMicros = 5000100

  _workerIdList <- forM [1..procNumber n] $ \i ->
    forkWorker defWorkerOptions { workerName = "Worker (" <> show i <> ")"
                              , workerRestartStrategy = Permanent }
              (spawnNumbersProcess (numberWriter i))
              capataz

  void $ forkWorker defWorkerOptions { workerName = "Worker Killer" }
                   (forever $ threadDelay delayMicros >> killNumberProcess)
                   capataz

  wait (capatazToAsync capataz)
    `finally`
    (teardown capataz >>= print)

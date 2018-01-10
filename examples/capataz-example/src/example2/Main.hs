{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Capataz
    ( CapatazOptions (..)
    , SupervisorRestartStrategy (..)
    , WorkerOptions (..)
    , WorkerRestartStrategy (..)
    , capatazToAsync
    , defCapatazOptions
    , defWorkerOptions
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
  capataz <- forkCapataz defCapatazOptions { supervisorName = "Example Capataz"
                                           , supervisorRestartStrategy = OneForOne
                                           , notifyEvent = pPrint
                                           }

  let numberWriter i a = print (i, a)
      delayMicros = 5000100

  _workerIdList <- forM [1 .. procNumber n] $ \i -> forkWorker
    defWorkerOptions { workerName            = "Worker (" <> show i <> ")"
                     , workerRestartStrategy = Permanent
                     }
    (spawnNumbersProcess (numberWriter i))
    capataz

  void $ forkWorker defWorkerOptions { workerName = "Worker Killer" }
                    (forever $ threadDelay delayMicros >> killNumberProcess)
                    capataz

  wait (capatazToAsync capataz) `finally` (teardown capataz >>= print)

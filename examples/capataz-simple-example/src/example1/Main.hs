{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RIO

import Lib             (Cli (..), killNumberProcess, procNumber, spawnNumbersProcess)
import Options.Generic (getRecord)


main :: IO ()
main = do
  logOptions <- logOptionsHandle stdout True
  withLogFunc logOptions $ \logFunc -> runRIO logFunc $ do

    n <- liftIO $ getRecord "Counter spawner"

    let numberWriter i a = logInfo $ displayShow (i :: Int, a :: Int)
        delayMicros = 5000100

    _asyncList <- forM [1 .. procNumber n]
      $ \i -> async $ spawnNumbersProcess (numberWriter i)

    killerAsync <-
      async $ forever $ threadDelay delayMicros >> killNumberProcess

    wait killerAsync

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Protolude
import Options.Generic (getRecord)
import Control.Concurrent.Async (async)
import Lib (Cli(..), SimpleProcess(..), spawnNumbersProcess, killNumberProcess)

main :: IO ()
main = do
  n <- getRecord "Counter spawner"

  let numberWriter i a = print (i, a)
      delayMicros = 5000100

  _asyncList <- forM [1..procNumber n] $ \i ->
    async $ spawnNumbersProcess (numberWriter i)

  killerAsync <-
    async $ forever $ threadDelay delayMicros >> killNumberProcess

  wait killerAsync

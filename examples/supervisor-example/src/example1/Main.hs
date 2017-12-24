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
import Lib (Cli(..), SimpleProcess(..), readNumbers)

main :: IO ()
main = do
  n <- getRecord "Counter spawner"
  asyncList <- forM [0..procNumber n] $ \i ->
    async $ readNumbers (\a -> print (i, a))
  void $ waitAny asyncList

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion
import Criterion.Main
import Protolude

import Lib (inc)

main :: IO ()
main = defaultMain [bench "inc 41" (whnf inc (41 :: Int))]

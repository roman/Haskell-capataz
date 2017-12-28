{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Protolude
import Options.Generic (getRecord)
import Control.Concurrent.Supervisor
  ( ChildOptions(..)
  , SupervisorOptions(..)
  , ChildRestartStrategy(..)
  , SupervisorRestartStrategy(..)
  , forkSupervisor
  , forkChild
  , defChildOptions
  , defSupervisorOptions
  , supervisorToAsync
  , teardown
  )
import Lib (Cli(..), spawnNumbersProcess, killNumberProcess)
import Text.Show.Pretty (pPrint)


main :: IO ()
main = do
  n <- getRecord "Counter spawner"
  supervisor <-
    forkSupervisor defSupervisorOptions { supervisorName = "Example Supervisor"
                                        , supervisorRestartStrategy = OneForOne
                                        , notifyEvent = pPrint }

  let numberWriter i a = print (i, a)
      delayMicros = 5000100

  _childIdList <- forM [1..procNumber n] $ \i ->
    forkChild defChildOptions { childName = "Worker (" <> show i <> ")"
                              , childRestartStrategy = Permanent }
              (spawnNumbersProcess (numberWriter i))
              supervisor

  void $ forkChild defChildOptions { childName = "Worker Killer" }
                   (forever $ threadDelay delayMicros >> killNumberProcess)
                   supervisor

  wait (supervisorToAsync supervisor)
    `finally`
    (teardown supervisor >>= print)

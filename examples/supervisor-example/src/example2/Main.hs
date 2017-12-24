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
import Lib (Cli(..), readNumbers)

main :: IO ()
main = do
  n <- getRecord "Counter spawner"
  supervisor <- forkSupervisor defSupervisorOptions { supervisorName = "Example Supervisor"
                                                    , supervisorRestartStrategy = AllForOne
                                                    , notifyEvent = print }

  _childIdList <- forM [1..procNumber n] $ \i ->
    forkChild defChildOptions { childName = "Worker (" <> show i <> ")"
                              , childRestartStrategy = Permanent }
              (readNumbers (\a -> print (i, a)))
              supervisor

  wait (supervisorToAsync supervisor)
    `finally`
    (teardown supervisor >>= print)

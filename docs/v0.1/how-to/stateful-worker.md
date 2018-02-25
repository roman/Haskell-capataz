# Create a stateful worker

## Description

In this example we implement a naive polish calculator, and it stores it's
calculator state every time an actor fails or terminates.

The following example:

* defines a set of operations that can be performed to worker via the
  `CalculatorOperation` record
* reads a backup from a filepath everytime it runs
* writes backup to a filepath everytime it is terminated (using `workerOnTerminationL`)
* writes backup to a filepath everytime it fails (using `workerOnFailureL`)
* creates a worker spec with behavior defined above
* creates a capataz system and a worker (using worker specification)
* executes a few operations to showcase calculator usage
* waits a few milliseconds to give the calculator worker some leeway
* tears down the capataz system and show results of teardown operation

## Code

```haskell
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Concurrent.Capataz.HowTo.StaticWorker where

import Control.Concurrent         (threadDelay)
import Control.Concurrent.Capataz
    ( ProcessSpec
    , WorkerRestartStrategy (..)
    , forkCapataz
    , set
    , supervisorProcessSpecListL
    , teardown
    , workerOnFailureL
    , workerOnTerminationL
    , workerRestartStrategyL
    , workerSpec
    )
import Control.Concurrent.STM     (atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Monad              (forever)
import Data.IORef                 (IORef, atomicModifyIORef, newIORef, readIORef)
import Data.Maybe                 (fromMaybe)
import GHC.Generics               (Generic)
import Safe                       (readMay)
import System.Directory           (doesFileExist)

data CalculatorOperation
  = Push Int
  | Add
  | Multiply
  | Top (Maybe Int -> IO ())
  | Read ([Int] -> IO ())
  deriving (Generic)

buildCalculatorWorker :: FilePath -> IO CalculatorOperation -> IO ProcessSpec
buildCalculatorWorker prevStBackup getOperation = do
  st    <- restoreBackup
  stRef <- newIORef st
  return $ workerSpec
    "calculator-worker"
    (workerLoop stRef)
    ( set workerRestartStrategyL Permanent
    . set workerOnFailureL       (const $ storeBackup stRef)
    . set workerOnTerminationL   (storeBackup stRef)
    )
 where
  restoreBackup :: IO [Int]
  restoreBackup = do
    filePresent <- doesFileExist prevStBackup
    if filePresent
      then (fromMaybe [] . readMay) <$> readFile prevStBackup
      else return []

  storeBackup :: IORef [Int] -> IO ()
  storeBackup stRef = do
    st <- readIORef stRef
    writeFile prevStBackup (show st)

  workerLoop :: IORef [Int] -> IO ()
  workerLoop stRef = forever $ do
    operation <- getOperation
    case operation of
      Push n -> atomicModifyIORef stRef (\ns -> (n : ns, ()))

      Add    -> atomicModifyIORef
        stRef
        ( \ns -> case ns of
          (a:b:rest) -> (a + b : rest, ())
          _          -> (ns, ())
        )

      Multiply -> atomicModifyIORef
        stRef
        ( \ns -> case ns of
          (a:b:rest) -> (a * b : rest, ())
          _          -> (ns, ())
        )

      Top notifyFn -> do
        result <- readIORef stRef
        case result of
          []    -> notifyFn Nothing
          (a:_) -> notifyFn (Just a)

      Read notifyFn -> do
        st <- readIORef stRef
        notifyFn st


main :: IO ()
main = do
  calculatorOpsQueue <- newTQueueIO
  let (readOperation, sendOperation) =
        ( atomically $ readTQueue calculatorOpsQueue
        , atomically . writeTQueue calculatorOpsQueue
        )

  myWorker <- buildCalculatorWorker "/tmp/calculator-state.bin" readOperation

  capataz  <- forkCapataz "stateful-worker-example"
                          (set supervisorProcessSpecListL [myWorker])

  sendOperation (Read print)
  sendOperation (Push 2)
  sendOperation (Push 2)
  sendOperation Add
  sendOperation (Top print)
  sendOperation (Push 8)
  sendOperation Multiply
  sendOperation (Top print)

  threadDelay 500

  result <- teardown capataz
  print result
```

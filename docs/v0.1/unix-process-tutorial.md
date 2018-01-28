# Unix Processes

We will build a small CLI application that spawns processes through Haskell's Unix Process API. We will keep all our threads running smoothly despite having one of the threads killing the spawned Unix processes through Unix `SIGTERM` signals.

We will implement two (2) different versions of our CLI program, one using standard Haskell threading utilities, and another using the Capataz library.

For this tutorial, we assume the reader is familiar with:

* [GHC Extensions](https://downloads.haskell.org/~ghc/8.2.2/docs/html/users_guide/glasgow_exts.html)
* [Stack](https://docs.haskellstack.org/en/stable/README/) projects
* [OptParse Generic library](https://hackage.haskell.org/package/optparse-generic) for CLI programs
* [Turtle library](https://hackage.haskell.org/package/turtle)
* [Haskell concurrency](http://chimera.labs.oreilly.com/books/1230000000929/pt02.html) (threads and STM)

If you are not familiar with the topics above, we recommend reading the tutorial while looking information from the given links.

## What you'll learn:

* How to add worker into a running supervision tree in a dynamic fashion
* How to spawn Unix processes from Haskell using Turtle
* How to kill Unix processes from Haskell using Turtle

## Summary of what our program will do

Our CLI program will receive an input parameter `procNumber` that will be used to spawn some green threads, each of them generating a Unix process. Each Unix process executes a simple bash script that echoes a number and increments it in an infinite while loop. Our Haskell program will also run a Haskell thread that sends an UNIX signal to one of the processes spawned by our program.

You can find the code for this tutorial in the [`examples`](https://github.com/roman/Haskell-capataz/tree/master/examples/) directory of the project's Github repository.

## Setting up the stage - A trivial library for Processes

Let's start by explaining the `Lib` module; it contains utility functions to spawn and kill Unix processes, first the header:

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Lib where

import qualified Data.ByteString.Char8 as C
import           Data.List             ((!!))
import qualified Data.Text             as T
import           Options.Generic       (ParseRecord)
import           Protolude
import           System.IO             (hGetLine, hIsEOF)
import qualified System.Process        as Process
import qualified System.Random         as Random
import qualified Turtle

-- (0)
newtype Cli =
  Cli { procNumber :: Int }
  deriving (Generic, Show)

instance ParseRecord Cli

-- (1)
data SimpleProcess =
  SimpleProcess { readStdOut       :: !(IO (Either ExitCode ByteString))
                , terminateProcess :: !(IO ())
                , waitProcess      :: !(IO ExitCode)
                }

-- (1)
spawnSimpleProcess :: Text -> [Text] -> IO SimpleProcess
spawnSimpleProcess program args = do
  let processSpec = (Process.proc (T.unpack program) (fmap T.unpack args))
        { Process.std_out = Process.CreatePipe
        }

  (_, Just hout, _, procHandle) <- Process.createProcess processSpec

  let readStdOut :: IO (Either ExitCode ByteString)
      readStdOut = do
        isEof <- hIsEOF hout
        if not isEof
          then (Right . C.pack) <$> hGetLine hout
          else Left <$> Process.waitForProcess procHandle

      terminateProcess :: IO ()
      terminateProcess = Process.terminateProcess procHandle

      waitProcess :: IO ExitCode
      waitProcess = Process.waitForProcess procHandle

  return SimpleProcess {readStdOut , terminateProcess , waitProcess }
```

`(0)` We have a `Cli` record that we use to gather values for our CLI program. Using the [optparse-generic](https://hackage.haskell.org/package/optparse-generic) library, this becomes a trivial affair. We make this work by adding an instance for `Generic` and `ParseRecord`.

`(1)` We create a `SimpleProcess` record. This record contains the logic to read the `stdout` of the spawned process and provides sub-routines  for _terminating_ or _waiting_ for termination of the Unix process. This utility record limits the scope of the Haskell Unix process API to our small use case.

Next, we implement the function that will spawn a Unix process that performs an `echo` of numbers from 1 to infinity in bash:

```haskell
spawnNumbersProcess
  :: (Int -> IO ())  -- ^ sub-routine that writes number to other resource
  -> IO ()
spawnNumbersProcess writeNumber = do
  -- We are going to execute a while loop that echoes numbers to stdout
  proc' <-
      spawnSimpleProcess
        "/bin/bash"
        ["-c"
        , "COUNTER=1; while [ $COUNTER -gt 0 ]; do "
          <>  "echo $COUNTER; sleep 1; let COUNTER=COUNTER+1; "
          <> "done"
        ]

  let loop = do
        -- read a string from stdout and transform it into a number, this sub-routine
        -- returns an Either where the Right value is a an stdout line, and the
        -- Left value is an exit code, in case the exit code is not a success, finish
        -- with an exception
        case eInput of
          Left exitCode
            | exitCode == ExitSuccess -> return ()
            | otherwise -> throwIO exitCode
          Right Nothing -> do
            putText "didn't get a number?"
            loop
          Right (Just number) -> do
            writeNumber number
            loop

  -- Make sure we terminate the process if we stop the loop using
  -- an async exception
  loop `finally` terminateProcess proc'
```

Now, let's have another `IO` sub-routine that lists Unix processes PIDs and picks one of them at random to send a `SIGTERM` signal. We use the [`Turtle` library](https://hackage.haskell.org/package/turtle) to run bash commands, in particular, the function (`procStrict`) which returns the `stdout` and `exitCode` of a process.

```haskell
processKiller
  :: Text  -- ^ Search processes with given name
  -> IO ()
processKiller processName = do
  (_exitCode, pgrepOutput) <-
      Turtle.procStrict "pgrep" ["-f", processName] Turtle.empty
      -- pgrep lists all pids from processes that have a particular name

  -- Split output in lines, and get pid per line
  let procNumbers = T.lines pgrepOutput
  case procNumbers of
    [] -> return ()
    _ -> do
      -- get a random element from the list of proccess identifiers
      theOneToKill <- Random.randomRIO (0, pred $ length procNumbers)

      putText $ "Process running: " <> show procNumbers
      putText $ "Killing: " <> (procNumbers !! theOneToKill)

      void $ Turtle.procStrict "kill" [procNumbers !! theOneToKill] Turtle.empty
```

## Example 1 - Running program without supervision

Once we have the API that spawns Unix processes, we implement a concurrent application that generates Haskell threads and calls this API; we build each thread using the standard [async](https://hackage.haskell.org/package/async) package:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Protolude -- (0)
import Options.Generic (getRecord)
import Control.Concurrent.Async (async)
import Lib (Cli(..), SimpleProcess(..), spawnNumbersProcess, killNumberProcess)

main :: IO ()
main = do
  n <- getRecord "Counter spawner" -- (1)

  let numberWriter i a = print (i, a)
      delayMicros = 5000100

  asyncList <- forM [1..procNumber n] $ \i ->
    async $ spawnNumbersProcess (numberWriter i) -- (2)

  killerAsync <-
    async $ forever $ threadDelay delayMicros >> killNumberProcess

  wait killerAsync `finally` mapM_ cancel asyncList
```

`(0)` We start by removing the default `Prelude` and use the batteries included [`protolude`](https://hackage.haskell.org/package/protolude) library, this provides most of the used functions from Haskell and some extra useful libraries.

`(1)` We use the [`optparse-generic`](https://hackage.haskell.org/package/optparse-generic) library to get a quick CLI optparser that provides us with the number of processes to run.

`(2)` We spawn an [async](https://hackage.haskell.org/package/async) (thread) where each of them is going to execute the `spawnProcessNumber` sub-routine.

`(3)` We spawn another thread that kills Unix processes.

When we run the previous program, it will fail slowly, removing the output of each of the threads that stop working after receiving an asynchronous exception. The Operative System throws this exception, but it originates from the execution of the `killerNumberProcess` sub-routine which sends a `SIGTERM` signal to the spawned process on each of the running threads.

The next example will show a project that uses the same functions, but relies on our API, which restarts threads in case of failure from external factors (in this case a `SIGTERM` Unix signal).

## Example 2 - Running program with supervision

Now, instead of using async, let's build the Haskell threads using a capataz instance that monitors both a group of threads that execute the `spawnProcessNumber` sub-routine and, a thread that terminates particular Unix process in a random fashion.

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Capataz -- (0)
    ( SupervisorRestartStrategy (..)
    , WorkerRestartStrategy (..)
    , buildWorkerOptions
    , buildWorkerOptionsWithDefaults
    , joinCapatazThread
    , forkCapataz
    , forkWorker
    , onSystemEventL
    , set
    , supervisorRestartStrategyL
    , teardown
    , workerRestartStrategyL
    )
import Lib                        (Cli (..), killNumberProcess, spawnNumbersProcess)
import Options.Generic            (getRecord)
import Protolude
import Text.Show.Pretty           (pPrint)

main :: IO ()
main = do
  n <- getRecord "Counter spawner"

  capataz <-
    forkCapataz -- (1)
      ( set supervisorRestartStrategyL OneForOne -- (2)
      . set onSystemEventL pPrint                -- (3)
      )

  let numberWriter i a = print (i, a)
      delayMicros = 5000100

  _workerIdList <- forM [1 .. procNumber n] $ \i -> do
      let
        counterWorkerOptions =
          buildWorkerOptions -- (4)
            ("Worker (" <> show i <> ")")
            (spawnNumbersProcess (numberWriter i)) -- (5)
            (set workerRestartStrategyL Permanent) -- (6)

      forkWorker -- (7)
        counterWorkerOptions
        capataz

  let
    workerKillerOptions =
        buildWorkerOptionsWithDefaults -- (8)
          "worker-killer"
          (forever $ threadDelay delayMicros >> killNumberProcess)

  -- ignore returned ProcessId, as we won't use it in our example
  void $ forkWorker workerKillerOptions capataz

  joinCapatazThread capataz  -- (9)
    `finally`
    (teardown capataz >>= print) -- (10)
```

We start the `main` sub-routine building a capataz instance, using the `forkCapataz` function.

`(0)` We import many symbols from our Capataz library; we need to provide some settings that will determine the restart mechanisms for the threads we want to keep running despite any errors that could happen.

`(1)` We use the `forkCapataz` function to start the capataz system; this call returns a (root) supervisor that can be used to dynamically create a supervision tree with supervisors and/or workers.

`(2)` We use the `supervisorRestartStrategyL` lens to override the default restart strategy. Some of the values may be:

* `OneForOne` -- if a monitored sub-routine thread fails, the supervisor will only restart the failing thread.

* `AllForOne` -- if a monitored sub-routine thread fails, the supervisor will restart all sub-routines that are monitored by it.

`(3)` We use the `onSystemEventL` lens to override the default callback. This callback gets called everytime something happens inside the supervisor; in this simple example, we are using the `pPrint` (pretty print) function to debug the capataz instance execution.

`(4)` We use the `buildWorkerOptions` function, we need this function to create a `WorkerOptions` record which requires three (3) arguments:

1. The worker name, which is used on the events triggered to the `onSystemEvent` callback.

2. The `IO ()` sub-routine to be executed on a supervised green thread.

3. A function that allows us to modify the default settings of `WorkerOptions`

`(5)` We pass our `spawnProcessNumber` IO sub-routine to execute it in a supervised green thread. This approach is no different from using a `forkIO`. Note the supervisor instance created in step `(1)` is the last parameter to the `forkWorker` function.

`(6)` We use the `workerRestartStrategyL` lens to override the default worker restart strategy, possible values are:

* `Permanent` -- The worker thread will _always_ get restarted, even it finishes without any errors. This strategy is ideal to monitor long-running servers.

* `Transient` -- The worker thread gets restarted, if and only if it fails with an error, if it completes without any errors, the supervisor will drop the worker thread from its supervision. This strategy is ideal to monitor one-time execution `IO ()` sub-routines.

* `Temporary` -- The worker thread will not be restarted, even in the case of failure, used for non-important sub-routines.

`(7)` We continue the example by spawning a few _workers_. For this, we use the `forkWorker` function which receives the `WorkerOptions` record created on step `(4)`.

`(8)` We create another `WorkerOptions` record with the `buildWorkerOptionsWithDefaults` function, which is similar to `builderWorkerOptions` but doesn't allow overrides to the default options.

`(9)` We join the current thread with the capataz' root supervisor thread.

`(10)` We make sure that we clean up the capataz system and supervised sub-routine threads using the [`teardown`](https://hackage.haskell.org/package/teardown) API.

## Try it out!

1) Clone the [capataz repository](https://github.com/roman/Haskell-capataz)

1) Run `make run-example1`

2) Run `make run-example2`

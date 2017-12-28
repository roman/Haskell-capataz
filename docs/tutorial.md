# Basic Tutorial using Unix Processes

In this (contrived) tutorial, we will build a small CLI app that spawns processes through Haskell's Process API, and keep our program running smoothly despite having another thread killing those processes using Unix Signals.

To get started, we are going to implement our CLI program without the supervisor library, and we'll add supervisor as we work on this.

For this tutorial, we assume the reader is familiar with the following:

* [GHC Extensions](https://downloads.haskell.org/~ghc/8.2.2/docs/html/users_guide/glasgow_exts.html)
* [Stack](https://docs.haskellstack.org/en/stable/README/) projects
* [OptParse](https://hackage.haskell.org/package/optparse-generic) for CLI programs
* [Haskell concurrency](http://chimera.labs.oreilly.com/books/1230000000929/pt02.html) (threads and STM)

If you are not familiar with the topics above, we recommend following the links listed.

## Summary of what our program will do

We are going to implement a CLI program that spawns a given amount of processes that executes a bash script that prints a number and increments it in a recurring fashion. Our Haskell program will also run a Haskell thread that will kill one of the many bash script executions.

You can find the code for this tutorial in the [`examples` directory](https://github.com/roman/Haskell-supervisor/tree/examples/examples) of the project's repository.

## Setting up the stage

Let's start by showcasing an `IO` sub-routine that spawns a UNIX process and reads its stdout.

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
        -- read number and transform it into a number, this function returns
        -- an Either where Right value is a an stdout line, and Left value is
        -- an exit code
        eInput <- ((readMaybe . BS.unpack) <$>) <$> readStdOut proc'
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

Second, let's have another `IO` sub-routine that lists processes pids, and kills on of them randomly. We use the [`Turtle` library](https://hackage.haskell.org/package/turtle) to run commands, we use a single function (`procStrict`) that returns the stdout and `exitCode` of a process.

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

The way we implement a concurrent application using the previous functions would be something like the following:

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

`(0)` We start by removing the default `Prelude` and use the batteries included [`protolude`](https://hackage.haskell.org/package/protolude) library, this provides most of the used functions from Haskell and some extra useful libraries

`(1)` We use the [`optparse-generic`](https://hackage.haskell.org/package/optparse-generic) library to get a quick CLI optparser that provides us with the number of processes to run

`(2)` We spawn an [async](https://hackage.haskell.org/package/async) (thread) where each of them is going to execute the `spawnProcessNumber` sub-routine

`(3)` We spawn another thread that kills Unix processes

When executed, this program is going to fail silently, removing the output of each of the threads that fail from the execution of the `killerNumberProcess` sub-routine. The following example will show a similar project that uses our API and adds reliability to the thread execution by restarting threads in case of failure from external factors.

## Example 2 - Running program with supervision

Now, let's have a supervisor that monitors both a group of threads that execute the `spawnProcessNumber` sub-routine and also another thread that kills process randomly.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Protolude
import Options.Generic (getRecord)
import Control.Concurrent.Supervisor -- (0)
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
    --                     (1)
    --                      |
    forkSupervisor defSupervisorOptions { supervisorName = "Example Supervisor"
                                        , supervisorRestartStrategy = OneForOne -- (2)
                                        , notifyEvent = pPrint                  -- (3)
                                        }

  let numberWriter i a = print (i, a)
      delayMicros = 5000100

  forM_ [1..procNumber n] $ \i ->
    --              (4)
    --               |
    void $ forkChild defChildOptions { childName = "Worker (" <> show i <> ")"
                                     , childRestartStrategy = Permanent  -- (5)
                                     }
                     (spawnNumbersProcess (numberWriter i)) -- (6)
                     supervisor

  let delayMicros = 5000100
  void $ forkChild defChildOptions { childName = "Worker Killer" }
                   (forever $ threadDelay delayMicros >> processKiller "while")
                   supervisor

  wait (supervisorToAsync supervisor)  -- (7)
    `finally`
    (teardown supervisor >>= print)    -- (8)
```

We start the `main` sub-routine building a supervisor using the `forkSupervisor` function.

`(0)` We start by importing a lot of symbols from our Supervisor library; when using this library, we need to provide many settings that will define the restart mechanisms for the threads we want to keep running despite errors

`(1)` its first argument is a set of options for the supervisor, in this example, we are overwriting some of them

`(2)` One of the options we overwrite is the `supervisorRestartStrategy`. Currently, the possible values may be:

* `OneForOne` -- if a monitored sub-routine fails, the supervisor will only restart the failing one

* `AllForOne` -- if a monitored sub-routine fails, the supervisor will restart all sub-routines that are currently monitoring

`(3)` The `notifyEvent` function will emit a `SupervisorEvent` record that emits events that indicate the current status of the supervisor, in this simple example, we are using the `pPrint` (pretty print) function to debug it.

We continue the example by spawning a few child sub-routines, for this, we use the `forkChild` function.

`(4)` As well as the `forkSupervisor` function, `forkChild` receives an options record, in this example, we are setting an explicit name for the children.

`(5)` We are also specifying the `childRestartStrategy`; this can be:

* Permanent -- The sub-routine will _always_ get restarted, even it finishes without any errors. This strategy is ideal to monitor servers.

* Transient -- The sub-routine gets restarted, if and only if it fails, if it completes without any errors, the supervisor will drop the sub-routine. This strategy is ideal to monitor one time executions.

* Temporary -- The sub-routine will not be restarted, even in the cause of failure, used for non-important executions.

`(6)` Then, we pass an `IO` sub-routine to execute in a monitored thread (in this particular example, our number process spawner). This approach is no different from using `forkIO`. Note the supervisor created on step (1) is the last parameter to the `forkChild` function.

`(7)` We can convert the supervisor sub-routine into an [`async`](https://hackage.haskell.org/package/async)

`(8)` We made sure that we cleaned up the supervisor and supervised sub-routine threads using the [`teardown`](https://hackage.haskell.org/package/teardown) API.

## Try it out!

1) Clone the [supervisor repository](https://github.com/roman/Haskell-supervisor)

1) Run `make run-example1`

2) Run `make run-example2`

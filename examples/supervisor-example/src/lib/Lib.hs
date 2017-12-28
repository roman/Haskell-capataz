{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Lib where

import Protolude
import System.IO (hGetLine, hIsEOF)
import Options.Generic (ParseRecord)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.List ((!!))
import qualified System.Process as Process
import qualified System.Random as Random
import qualified Turtle

data Cli =
  Cli { procNumber :: !Int }
  deriving (Generic, Show)

instance ParseRecord Cli

data SimpleProcess =
  SimpleProcess { readStdOut       :: !(IO (Either ExitCode ByteString))
                , terminateProcess :: !(IO ())
                , waitProcess      :: !(IO ExitCode)
                }

spawnSimpleProcess :: Text -> [Text] -> IO SimpleProcess
spawnSimpleProcess program args = do
  let
    processSpec =
      (Process.proc (T.unpack program) (map T.unpack args)) {
        Process.std_out = Process.CreatePipe
      }

  (_, Just hout, _, procHandle) <- Process.createProcess processSpec

  let
    readStdOut :: IO (Either ExitCode ByteString)
    readStdOut = do
      isEof <- hIsEOF hout
      if not isEof then
        (Right . BS.pack) <$> hGetLine hout
      else
        Left <$> Process.waitForProcess procHandle

    terminateProcess :: IO ()
    terminateProcess =
      Process.terminateProcess procHandle

    waitProcess :: IO ExitCode
    waitProcess = Process.waitForProcess procHandle

  return SimpleProcess { readStdOut, terminateProcess, waitProcess }

processKiller :: Text -> IO ()
processKiller processName = do
  (_, pgrepOutput) <- Turtle.procStrict "pgrep" ["-f", processName] Turtle.empty
  let procNumbers = T.lines pgrepOutput
  case procNumbers of
    [] -> return ()
    _ -> do
      theOneToKill <- Random.randomRIO (0, pred $ length procNumbers)
      putText $ "Process running: " <> show procNumbers
      putText $ "Killing: " <> (procNumbers !! theOneToKill)
      void $ Turtle.procStrict "kill" [procNumbers !! theOneToKill] Turtle.empty

--------------------------------------------------------------------------------

spawnNumbersProcess :: (Int -> IO ()) -> IO ()
spawnNumbersProcess writeNumber = do
  proc' <-
      spawnSimpleProcess
      "/bin/bash"
      ["-c", "COUNTER=1; while [ $COUNTER -gt 0 ]; do echo $COUNTER; sleep 1; let COUNTER=COUNTER+1; done"]

  let loop = do
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

  loop `finally` terminateProcess proc'

killNumberProcess :: IO ()
killNumberProcess = processKiller "while"

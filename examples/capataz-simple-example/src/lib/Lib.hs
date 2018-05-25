{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Lib where

import           RIO
import qualified RIO.Text             as T
import qualified RIO.ByteString.Lazy  as LB
import qualified RIO.Process          as Process

import RIO.List.Partial ((!!))

import Control.Concurrent.STM (orElse)

import           Options.Generic       (ParseRecord)
import qualified System.Random         as Random
-- import qualified Turtle

newtype Cli =
  Cli { procNumber :: Int }
  deriving (Generic, Show)

instance ParseRecord Cli

-- data SimpleProcess =
--   SimpleProcess { readStdOut       :: !(IO (Either ExitCode Text))
--                 , terminateProcess :: !(IO ())
--                 , waitProcess      :: !(IO ExitCode)
--                 }

-- spawnSimpleProcess :: Text -> [Text] -> IO SimpleProcess
-- spawnSimpleProcess program args = do
--   let processSpec = (Process.proc (T.unpack program) (fmap T.unpack args))
--         { Process.std_out = Process.CreatePipe
--         }

--   (_, Just hout, _, procHandle) <- Process.createProcess processSpec

--   let readStdOut :: IO (Either ExitCode Text)
--       readStdOut = do
--         isEof <- hIsEOF hout
--         if not isEof
--           then (Right . T.pack) <$> hGetLine hout
--           else Left <$> Process.waitForProcess procHandle

--       terminateProcess :: IO ()
--       terminateProcess = Process.terminateProcess procHandle

--       waitProcess :: IO ExitCode
--       waitProcess = Process.waitForProcess procHandle

--   return SimpleProcess {readStdOut , terminateProcess , waitProcess }

pgrepProc :: (MonadIO m, MonadReader env m, HasLogFunc env, Process.HasProcessContext env) => String -> (Process.ProcessConfig () () () -> m a) -> m a
pgrepProc processName =
  Process.proc "pgrep" ["-f", processName]

killProc :: (MonadIO m, MonadReader env m, HasLogFunc env, Process.HasProcessContext env) => Text -> (Process.ProcessConfig () () () -> m a) -> m a
killProc procId =
  Process.proc "kill" [T.unpack procId]

processKiller :: MonadUnliftIO m => String -> m ()
processKiller processName = do
  logOptions <- logOptionsHandle stdout True
  processCtx <- Process.mkDefaultProcessContext
  withLogFunc logOptions $ \logFunc ->
    runRIO (Process.LoggedProcessContext processCtx logFunc) $ do
      pgrepProc processName $ \pgrepConfig -> do
        eoutput <- (T.decodeUtf8' . LB.toStrict) <$> Process.readProcessStdout_ pgrepConfig
        case eoutput of
          Left err ->
            logWarn $ "Encoding error: " <> displayShow err
          Right pgrepOutput -> do
            let procNumbers = T.lines pgrepOutput
            case procNumbers of
              [] -> return ()
              _  -> do
                theOneToKill <- liftIO $ Random.randomRIO (0, length procNumbers - 1)
                logInfo $ "Process running: " <> displayShow procNumbers
                logInfo $ "Killing: " <> display (procNumbers !! theOneToKill)
                killProc (procNumbers !! theOneToKill) $ \killConfig ->
                  Process.runProcess_ killConfig


killNumberProcess :: (MonadUnliftIO m, MonadIO m) => m ()
killNumberProcess = processKiller "while"

--------------------------------------------------------------------------------


counterProc :: (Process.ProcessConfig () () () -> RIO Process.LoggedProcessContext a) -> RIO Process.LoggedProcessContext a
counterProc =
    Process.proc "/bin/bash" [ "-c"
                              , "COUNTER=1; while [ $COUNTER -gt 0 ]; do "
                                <> "echo $COUNTER; sleep 1; let COUNTER=COUNTER+1; "
                                <> "done"
                              ]


spawnNumbersProcess
  :: ( HasLogFunc env
     , MonadReader env (RIO Process.LoggedProcessContext)
     , MonadUnliftIO m
     )
  => (Int -> RIO Process.LoggedProcessContext ())
  -> m ()
spawnNumbersProcess writeNumber = do
  logOptions <- logOptionsHandle stdout True
  processCtx <- Process.mkDefaultProcessContext
  withLogFunc logOptions $ \logFunc ->
    runRIO (Process.LoggedProcessContext processCtx logFunc) $ do
      counterProc $ \pgrepConfig ->
        bracket (Process.startProcess $ pgrepConfig & Process.setStdout Process.byteStringOutput)
                 Process.stopProcess $ \process -> do
          let
            stmOut = Process.getStdout process
            readNumber = (fmap (readMaybe . T.unpack) . T.decodeUtf8' . LB.toStrict) <$> stmOut
            loop = do
              eInput <- atomically $ (Right <$> readNumber) `orElse` (Left <$> Process.waitExitCodeSTM process)
              case eInput of
                Left exitCode | exitCode == ExitSuccess -> return ()
                              | otherwise               -> throwM exitCode
                Right (Left err) ->
                  logError $ "Encoding error: " <> displayShow err
                Right (Right Nothing) -> do
                  logWarn $ "Didn't get a number?"
                Right (Right (Just number)) -> do
                  writeNumber number
                  loop
          loop

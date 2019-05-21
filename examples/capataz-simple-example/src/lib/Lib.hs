{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Lib where

import           RIO
import qualified RIO.ByteString.Lazy  as LB
import qualified RIO.Text             as T
import qualified System.Process.Typed as Process

import RIO.List.Partial ((!!))

import           Options.Generic (ParseRecord)
import qualified System.Random   as Random

newtype Cli =
  Cli { procNumber :: Natural }
  deriving (Generic, Show)

instance ParseRecord Cli


pgrepProc :: String -> Process.ProcessConfig () () ()
pgrepProc processName = Process.proc "pgrep" ["-f", processName]

killProc :: Text -> Process.ProcessConfig () () ()
killProc procId = Process.proc "kill" [T.unpack procId]

processKiller
  :: (HasLogFunc env, MonadUnliftIO m, MonadReader env m) => String -> m ()
processKiller processName = do
  eoutput <- T.decodeUtf8' . LB.toStrict <$> Process.readProcessStdout_
    (pgrepProc processName)
  case eoutput of
    Left  err         -> logWarn $ "Encoding error: " <> displayShow err
    Right pgrepOutput -> do
      let procNumbers = T.lines pgrepOutput
      case procNumbers of
        [] -> return ()
        _  -> do
          theOneToKill <- liftIO $ Random.randomRIO (0, length procNumbers - 1)
          logInfo $ "Process running: " <> displayShow procNumbers
          logInfo $ "Killing: " <> display (procNumbers !! theOneToKill)
          Process.runProcess_ (killProc (procNumbers !! theOneToKill))


killNumberProcess :: (HasLogFunc env) => RIO env ()
killNumberProcess = processKiller "while"

--------------------------------------------------------------------------------


counterProc :: Process.ProcessConfig () (STM LB.ByteString) ()
counterProc =
  Process.proc
      "/bin/bash"
      [ "-c"
      , "COUNTER=1; while [ $COUNTER -gt 0 ]; do "
      <> "echo $COUNTER; sleep 1; let COUNTER=COUNTER+1; "
      <> "done"
      ]
    & Process.setStdout Process.byteStringOutput


spawnNumbersProcess :: (HasLogFunc env) => (Natural -> RIO env ()) -> RIO env ()
spawnNumbersProcess writeNumber =
  bracket (Process.startProcess counterProc) Process.stopProcess
    $ \countProcess -> do

        let
          stmOut = Process.getStdout countProcess

          readNumber =
            fmap (readMaybe . T.unpack) . T.decodeUtf8' . LB.toStrict <$> stmOut

          loop = do
            eInput <-
              atomically
              $   (Right <$> readNumber)
              <|> (Left <$> Process.waitExitCodeSTM countProcess)

            case eInput of
              Left exitCode | exitCode == ExitSuccess -> return ()
                            | otherwise               -> throwM exitCode

              Right (Left err) ->
                logError $ "Encoding error: " <> displayShow err

              Right (Right Nothing      ) -> logWarn "Didn't get a number?"

              Right (Right (Just number)) -> do
                writeNumber number
                loop

        loop

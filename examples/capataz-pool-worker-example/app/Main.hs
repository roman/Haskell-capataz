{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Capataz
import RIO     hiding (link)

import qualified RIO.ByteString.Lazy as LB
import qualified RIO.Set             as Set
import qualified RIO.Text            as Text

import Network.URI (uriAuthority, uriRegName, uriScheme)
-- import Network.HTTP.Client (Manager, Request, parseRequest, httpLbs, responseStatus, responseBody, getUri)
-- import Network.HTTP.Types.Status (statusCode)

#if MIN_VERSION_http_client_tls(0,2,4)
import Network.HTTP.Client
    (Manager, Request, getUri, httpLbs, newManager, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
#else
import Network.HTTP.Client
    (Manager, Request, getUri, httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
#endif

import           Text.HandsomeSoup (css, parseHtml, (!))
import           Text.XML.HXT.Core ((>>>))
import qualified Text.XML.HXT.Core as HXT

inferUrlDomain :: Request -> String -> Maybe Text
inferUrlDomain req linkStr =
  let
    link = Text.pack linkStr
    uri  = getUri req
  in
    if Text.isPrefixOf "mailto:" link
      then Nothing
      else if Text.isPrefixOf "//" link
        then Just $ Text.pack (uriScheme uri) <> link
        else if Text.isPrefixOf "/" link || Text.isPrefixOf "./" link
          then case uriRegName <$> uriAuthority uri of
            Just domain ->
              Just
                $  Text.pack (uriScheme uri)
                <> "//"
                <> Text.pack domain
                <> link  -- Use domain from the req

            Nothing -> Nothing
          else
            if Text.isPrefixOf "http://" link || Text.isPrefixOf "https://" link
              then Just link
              else Nothing

fetchUrlList
  -- :: (HasLogFunc env, MonadReader env m, MonadIO m)
  :: (MonadReader env m, MonadIO m)
  => Manager
  -> Text
  -> m (Either UnicodeException [Text])
fetchUrlList manager url = do
  request  <- liftIO $ parseRequest (Text.unpack url)
  -- logInfo $ "Perform request to URL: " <> display url
  response <- liftIO $ httpLbs request manager
  -- logInfo $ "Response accquired:"
  -- logInfo $ "  status: " <> display (statusCode $ responseStatus response)
  -- logInfo $ "  length: " <> display (LB.length $ responseBody response)
  -- logInfo $ displayShow (responseBody response)
  let edoc = (parseHtml . Text.unpack)
        <$> decodeUtf8' (LB.toStrict (responseBody response))
  case edoc of
    Left  err -> return $ Left err
    Right doc -> do
      links <- liftIO $ HXT.runX (doc >>> css "a" ! "href")
      -- logInfo $ "Links accquired: " <> display (length links)
      return $ Right $ take 10 $ catMaybes $ map (inferUrlDomain request) links

processUrlWorker
  :: (HasLogFunc env, MonadIO m, MonadReader env m)
  => Manager
  -> IORef (Set Text)
  -> (Text -> m ())
  -> WorkerId
  -> Text
  -> m ()
processUrlWorker manager visitedUrlsRef addLink workerId url = do
  shouldContinue <- atomicModifyIORef' visitedUrlsRef $ \urlSet ->
    if Set.member url urlSet
      then (urlSet, False)
      else (Set.insert url urlSet, True)

  if shouldContinue
    then do
      logInfo
        $  "Worker "
        <> displayShow workerId
        <> " processing "
        <> display url
      elinks <- fetchUrlList manager url
      case elinks of
        Left err ->
          logError
            $  "Worker receiveid a response with invalid encoding "
            <> displayShow err
        Right links ->
          -- One URL at a time to not hog the manager; in an ideal world, addLink
          -- would store the link in a persistent layer
          mapM_ addLink (take 3 links)
    else
      logInfo $ "Worker " <> displayShow workerId <> " ignoring " <> display url

webCrawler
  :: (MonadIO m, MonadReader env m, HasLogFunc env, MonadIO m0)
  => Manager
  -> Text
  -> Int
  -> m0 (ProcessSpec m)
webCrawler manager startUrl workerCount = do
  -- Add a lot of room for the URLs in flight
  urlLinkQueue  <- newTBQueueIO (workerCount * 100)
  visitedSetRef <- newIORef Set.empty

  let getNextLink = atomically $ readTBQueue urlLinkQueue
      addLink link = atomically $ writeTBQueue urlLinkQueue link

  atomically $ writeTBQueue urlLinkQueue startUrl

  buildStealWorkerPoolSpec WorkerPoolArgs
    { poolSupervisorName    = "web-crawler"
    , poolSupervisorOptions = set supervisorRestartStrategyL OneForOne
          -- Fail the supervisor if half the workers fail in
          -- a period of 5 seconds
      . set supervisorIntensityL     (workerCount `div` 2)
      . set supervisorPeriodSecondsL 5
    , poolPullNewWork       = getNextLink
    , poolWorkerNamePrefix  = "crawler"
    , poolWorkerCount       = workerCount
    , poolWorkerAction      = processUrlWorker manager visitedSetRef addLink
    , poolWorkerOptions     = set workerRestartStrategyL Permanent
    }

main :: IO ()
main = do
  logOptions  <- logOptionsHandle stdout True
#if MIN_VERSION_http_client_tls(0,2,4)
  manager     <- newManager tlsManagerSettings
#else
  manager     <- newTlsManagerWith tlsManagerSettings
#endif
  crawlerSpec <- webCrawler manager genesysUrl 20
  withLogFunc logOptions $ \logFunc -> runRIO logFunc $ do
    capataz <- forkCapataz
      "capataz-worker-pool-example"
      ( set onSystemEventL             (logDebug . display)
      . set supervisorProcessSpecListL [crawlerSpec]
      )
    joinCapatazThread capataz

genesysUrl :: Text
genesysUrl = "http://www.parsonsmatt.org/"

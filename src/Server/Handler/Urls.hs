module Server.Handler.Urls
  ( shortenH
  , listUrlsH
  , deleteUrlH
  )
where

import           Servant
import           Servant.Auth.Server

import           Relude

import           Database.Common
import           Database.Database
import           Database.Url.Url    as U
import           Server.Types        as T
import           Server.Types.Util   (dbToServerError)
import           Server.Util

shortenH :: AuthResult T.User -> ShortenReqBody -> HandlerT ShortenedUrl
shortenH (Authenticated T.User{..}) b@ShortenReqBody{..} = do
  log <- asks logger
  logWith log $ "Shortening url: " <> show b

  tbs <- asks tables
  shUrl <- case srbAlias of
             Just al -> return al
             Nothing -> do
               uuidR <- liftIO . runDB tbs $ genUUID
               case uuidR of
                 Right uuid -> return $ shortenWithAlphabet alphabet uuid
                 Left e     -> throwError $ dbToServerError e
  logWith log $ "Got short url: " <> shUrl

  let url = U.Url srbUrl shUrl
  res <- liftIO . runDB tbs $ createUrl url userEmail
  case res of
    Right () -> do
      logWith log "Successfully shortened url"
      return $ ShortenedUrl shUrl
    Left  e  -> do
      logWith log $ "Got an error: " <> show e
      throwError $ dbToServerError e
shortenH _                          _                  = throwError err401

listUrlsH :: AuthResult T.User -> HandlerT [ShortUrl]
listUrlsH (Authenticated T.User{..}) = do
  log <- asks logger
  logWith log $ "Listing urls for user " <> show userEmail

  tbs <- asks tables
  res <- liftIO . runDB tbs $ getUserUrls userEmail
  case res of
    Right (Just urls) -> return urls
    Right Nothing     -> throwError err404 { errBody = "Can't find such user" }
    Left  e           -> throwError $ dbToServerError e
listUrlsH _                          = throwError err401


deleteUrlH :: AuthResult T.User -> Text -> HandlerT NoContent
deleteUrlH (Authenticated T.User{..}) alias = do
  log <- asks logger
  logWith log $ "Deleting url " <> alias <> " for user " <> userEmail

  tbs <- asks tables

  res <- liftIO . runDB tbs $ deleteUrl alias userEmail

  case res of
    Right () -> return NoContent
    Left  e  -> throwError $ dbToServerError e
deleteUrlH _                          _     = throwError err401

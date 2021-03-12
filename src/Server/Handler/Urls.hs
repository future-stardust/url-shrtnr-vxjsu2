module Server.Handler.Urls
  ( shortenH
  , listUrlsH
  , deleteUrlH
  )
where

import           Server.Types      as T

import           Database.Common
import           Database.Database
import           Database.Url.Url  as U
import           Relude
import           Servant
import           Server.Types.Util (dbToServerError)
import           Server.Util

shortenH :: ShortenReqBody -> Maybe Token -> HandlerT ShortenedUrl
shortenH _                  Nothing  = throwError err404 { errBody = "Cookie is missing" }
shortenH ShortenReqBody{..} (Just t) = do
  let d = tokenToData t
  name <- case d of
            Just (n, _) -> return n
            Nothing     -> throwError err500 { errBody = "Can't decode cookies" }

  tbs <- asks tables

  shUrl <- case srbAlias of
             Just al -> return al
             Nothing -> do
               uuidR <- liftIO . runDB tbs $ genUUID
               case uuidR of
                 Right uuid -> return $ shortenWithAlphabet alphabet uuid
                 Left e     -> throwError $ dbToServerError e

  let url = U.Url srbUrl shUrl
  res <- liftIO . runDB tbs $ createUrl url name
  case res of
    Right () -> return $ ShortenedUrl shUrl
    Left  e  -> throwError $ dbToServerError e

listUrlsH :: Maybe Token -> HandlerT [ShortUrl]
listUrlsH Nothing  = throwError err401
listUrlsH (Just t) = do
  let d = tokenToData t
  name <- case d of
            Just (n, _) -> return n
            Nothing -> throwError err500 { errBody = "Can't decode cookies" }
  tbs <- asks tables
  res <- liftIO . runDB tbs $ getUserUrls name
  case res of
    Right (Just urls) -> return urls
    Right Nothing     -> throwError err404 { errBody = "Can't find such user" }
    Left  e           -> throwError $ dbToServerError e


deleteUrlH :: Text -> Maybe Token -> HandlerT NoContent
deleteUrlH _     Nothing  = throwError err401
deleteUrlH alias (Just t) = do
  let d = tokenToData t
  name <- case d of
            Just (n, _) -> return n
            Nothing -> throwError err500 { errBody = "Can't decode cookies" }

  tbs <- asks tables

  res <- liftIO . runDB tbs $ deleteUrl alias name

  case res of
    Right () -> return NoContent
    Left  e  -> throwError $ dbToServerError e

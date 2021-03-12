module Server.Handler.Urls
  ( shortenH
  , listUrlsH
  , deleteUrlH
  )
where

import           Server.Types       as T

import           Data.List.NonEmpty ((!!))
import           Database.Common
import           Database.Database
import           Database.Url.Url   as U
import           Relude
import           Servant
import           Server.Types.Util  (dbToServerError)

shortenH :: ShortenReqBody -> Maybe Token -> HandlerT ShortenedUrl
shortenH _                  Nothing  = throwError 404 { errBody = "Cookie is missing" }
shortenH ShortenReqBody{..} (Just t) = do
  let d = tokenToData t
  name <- case d of
            Just (n, _) -> return n
            Nothing -> throwError err500 { errBody = "Can't decode cookies" }
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
  where
    alphabet = '0' :| (['1'..'9'] <> ['a'..'z'] <> ['A'..'Z'])
    base = length alphabet

    shortenWithAlphabet :: NonEmpty Char -> Int -> Text
    shortenWithAlphabet a 0 = toText [head a]
    shortenWithAlphabet a i = rest <> toText [digit]
      where
        digit = a !! (i `mod` base)
        remainder = i `div` base
        rest = if remainder > 0
               then shortenWithAlphabet a remainder
               else ""

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
  return NoContent

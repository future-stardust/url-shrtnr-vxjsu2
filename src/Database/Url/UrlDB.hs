{-# LANGUAGE TemplateHaskell #-}

module Database.Url.UrlDB
  ( insertUrl
  , queryUrl
  , deleteUrl
  , genUUID
  ) where

import Data.Acid
import Control.Monad.Except
import Relude hiding (find)
import Relude.Unsafe (fromJust)

import Database.Url.Url
import Database.User.User
import Database.User.UserDB
import Database.Common
import Database.State


-- | Inserts `Url` in db
insertUrl :: DB m => Url -> Username -> m ()
insertUrl url uname = do
  (userTable, urlTable) <- ask

  -- checks if url is not empty
  when (urlIsNil url) $ throwError EUrlNil
  -- checks if user with such username does exist
  user <- liftIO $ query userTable $ QueryUser' uname
  unless (isJust user) $ throwError EUserNExist
  -- check if shortenend url exists
  oldUrl <- liftIO $ query urlTable $ QueryUrl' $ short url
  when (isJust oldUrl) $ throwError EUrlExist

  let u = fromJust user
  liftIO $ update userTable $ UpdateUrlsUser' u $ short url
  liftIO $ update urlTable  $ InsertUrl' url

-- | Deletes `Url` from db
deleteUrl :: DB m => ShortUrl -> Username -> m ()
deleteUrl shortUrl uname = do
  (userTable, urlTable) <- ask
  user <- fromMaybe (error $ show EUserNExist) <$> queryUser uname

  liftIO $ update userTable $ DeleteUrlUser' user shortUrl
  liftIO $ update urlTable  $ DeleteUrl' shortUrl

-- | Searches for `OrigUrl` by given `ShortUrl`
queryUrl :: DB m => ShortUrl -> m (Maybe OrigUrl)
queryUrl url = do
  (_, urlTable) <- ask
  liftIO $ query urlTable $ QueryUrl' url

-- | Generates new `UUID`
genUUID :: DB m => m UUID
genUUID = do
  (_, urlTable) <- ask
  liftIO $ update urlTable UpdateUUID'
  liftIO $ query  urlTable QueryUUID'

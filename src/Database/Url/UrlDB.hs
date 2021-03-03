{-# LANGUAGE TemplateHaskell #-}

module Database.Url.UrlDB
  ( insertUrl
  , queryUrl
  , deleteUrl
  , genUUID
  ) where

import Control.Monad.Except
import Data.Acid
import Relude hiding (find)

import Database.Url.Url
import Database.User.UserDB
import Database.Common
import Database.State


-- | Inserts `Url` in db
insertUrl :: DB m => Url -> Username -> m ()
insertUrl url uname = do
  (_, urlTable) <- ask

  -- check if url is not empty
  when (urlIsNil url) $ throwError EUrlNil
  -- check if shortenend url exists
  urlExists <- isJust <$> queryUrl (short url)
  when urlExists $ throwError EUrlExist

  updateUrlsUser uname $ short url
  liftIO $ update urlTable $ InsertUrl' url

-- | Deletes `Url` from db
deleteUrl :: DB m => ShortUrl -> Username -> m ()
deleteUrl url uname = do
  (_, urlTable) <- ask

  -- check if url is not empty
  when (shortUrlIsNil url) $ throwError ESUrlNil
  -- check if such url exists
  urlExists <- isJust <$> queryUrl url
  unless urlExists $ throwError EUrlNExist

  deleteUrlUser uname url
  liftIO $ update urlTable $ DeleteUrl' url

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

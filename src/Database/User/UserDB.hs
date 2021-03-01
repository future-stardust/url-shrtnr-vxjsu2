module Database.User.UserDB where

import Data.Acid
import Relude

import Database.Common
import Database.User.User
import Database.State
import Relude.Unsafe (fromJust)
import Control.Monad.Error.Class

-- | Inserts `User` in db
insertUser :: DB m => User -> m ()
insertUser user@User{..} = do
  (userTable, _) <- ask

  -- checks if `User` doesn't have empty fields
  -- and dosen't exist in db
  unless (unameHashNotNil user) $ throwError EUserNil
  userExist <- queryUser username <&> (/= Nothing)
  when userExist $ throwError EUserExist

  liftIO $ update userTable $ InsertUser' user

-- | Adds url to `User`'s urls associated with him
updateUrlsUser :: DB m => Username -> ShortUrl -> m ()
updateUrlsUser uname url = do
  (userTable, _) <- ask

  -- checks if url is not empty
  when (shortUrlIsNil url) $ throwError ESUrlNil
  -- queries user
  user <- queryUser uname
  -- checks if it does exist
  let userExist = isJust user
  unless userExist $ throwError EUserNExist
  -- check if such url already exists
  let urlExists = fromJust $ elem url . urls <$> user
  when urlExists $ throwError ESUrlExist

  -- updates urls, fromJust is ok, because it's checked
  -- in the code above
  let u = fromJust user
  liftIO $ update userTable $ UpdateUrlsUser' u url

-- | Searches for `User` with given `Username`
queryUser :: DB m => Username -> m (Maybe User)
queryUser uname = do
  (userTable, _) <- ask
  liftIO $ query userTable $ QueryUser' uname

-- | Searces for `ShortUrl`s associated with `User`
queryUserUrls :: DB m => Username -> m (Maybe [ShortUrl])
queryUserUrls = (<$>) (urls <$>) . queryUser

-- | Deletes given `ShortUrl` from user with `Username`
deleteUrlUser :: DB m => Username -> ShortUrl -> m ()
deleteUrlUser uname url = do
  (userTable, _) <- ask

  -- checks if url is not empty
  when (shortUrlIsNil url) $ throwError ESUrlNil
  -- queries user
  user <- queryUser uname
  -- checks if it does exist
  let userExist = isJust user
  unless userExist $ throwError EUserNExist
  -- check if such url doesn't exist
  let urlExists = fromJust $ elem url . urls <$> user
  unless urlExists $ throwError ESUrlNExist

  -- updates urls, fromJust is ok, because it's checked
  -- in the code above
  let u = fromJust user
  liftIO $ update userTable $ DeleteUrlUser' u url

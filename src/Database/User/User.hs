{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.User.User
  ( TreeUser
  , UserTable
  , User(..)
  , InsertUser'(..)
  , QueryUser'(..)
  , UpdateUrlsUser'(..)
  , DeleteUrlUser'(..)
  , insertUser'
  , queryUser'
  , updateUrlsUser'
  , deleteUrlUser'
  , unameHashNotNil
  ) where


import Data.Acid
import Data.SafeCopy
import Relude hiding (find)

import Database.Common
import qualified Database.Tree.Tree as BT

-- | User representation in database
data User = User
  { username :: Username   -- ^ user's username -- Unique
  , urls     :: [ShortUrl] -- ^ shortened url created by user
  , hash     :: Hash       -- ^ password hash
  } deriving (Eq, Show)

-- | Tree for user table
type TreeUser  = BT.Tree Username User
type UserTable = Table TreeUser

instance BT.HasIndex User where
  type Index User = Username
  getIndex = username

instance SafeCopy User where
  putCopy User{..} = contain $ safePut username
                            >> safePut urls
                            >> safePut hash
  getCopy = contain $ User <$> safeGet <*> safeGet <*> safeGet

instance SafeCopy TreeUser where
  putCopy (BT.toList -> list) = contain $ safePut list
  getCopy = contain $ BT.fromList <$> safeGet


-- | Inserts `User` in db
insertUser' :: User -> Update UserTable ()
insertUser' = modify . (<$>) . BT.insert

-- | Adds given url to `User`'s `short` field
updateUrlsUser' :: User -> ShortUrl -> Update UserTable ()
updateUrlsUser' User{..} url =
  modify . (<$>) . BT.insert $ User username (urls <> [url]) hash

-- | Deletes given url from `User`'s `short` field
-- optimise double traverse because of User
deleteUrlUser' :: User -> ShortUrl -> Update UserTable ()
deleteUrlUser' User{..} url = do
  modify . (<$>) . BT.insert $ User username (filter (/= url) urls) hash

-- | Searches for `User` with given `Username` in db
queryUser' :: Username -> Query UserTable (Maybe User)
queryUser' uname = do
  Table tree <- ask
  return $ BT.lookup uname tree

$(makeAcidic ''UserTable ['insertUser', 'updateUrlsUser', 'queryUser', 'deleteUrlUser'])


------------ AUXILIARY FUNCTIONS ------------

-- | Checks if `User`'s username and hash are not empty
-- TODO: clarify if needed
unameHashNotNil :: User -> Bool
unameHashNotNil User{..} = username /= "" && hash /= ""

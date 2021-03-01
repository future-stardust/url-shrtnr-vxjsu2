{-# LANGUAGE TemplateHaskell #-}

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
import Relude hiding (find)
import Database.Common
import Data.SafeCopy
import qualified Data.Text as T
import Data.RedBlackTree
import Control.Exception

-- | User representation in database
data User = User
  { username :: Username   -- ^ user's username -- Unique
  , urls     :: [ShortUrl] -- ^ shortened url created by user
  , hash     :: T.Text     -- ^ password hash
  } deriving (Show)

-- | Tree for user table
type TreeUser  = RedBlackTree User
type UserTable = Table TreeUser

instance Ord User where
  (<=) (User u1 _ _) (User u2 _ _) = u1 <= u2
  (<)  (User u1 _ _) (User u2 _ _) = u1 <  u2
  (>)  (User u1 _ _) (User u2 _ _) = u1 >  u2

instance Eq User where
  (==) (User u1 _ _) (User u2 _ _) = u1 == u2

instance BinaryTreeNode User where
  mergeNodes _ = id

instance SafeCopy User where
  putCopy User{..} = contain $ safePut username
                            >> safePut urls
                            >> safePut hash
  getCopy = contain $ User <$> safeGet <*> safeGet <*> safeGet

instance SafeCopy TreeUser where
  putCopy = contain . safePut
  getCopy = contain   safeGet


-- | Inserts `User` in db
insertUser' :: User -> Update UserTable ()
insertUser' user = do
  Table tree <- get
  put $ Table $ insert tree user

-- | Adds given url to `User`'s `short` field
updateUrlsUser' :: User -> ShortUrl -> Update UserTable ()
updateUrlsUser' User{..} url = do
  modify $ fmap $ flip insert $ User username (url : urls) hash

-- | Deletes given url from `User`'s `short` field
-- optimise double traverse because of User
deleteUrlUser' :: User -> ShortUrl -> Update UserTable ()
deleteUrlUser' User{..} url = do
  modify $ (<$>) $ flip insert $ User username (filter (/= url) urls) hash

-- | Searches for `User` with given `Username` in db
queryUser' :: Username -> Query UserTable (Maybe User)
queryUser' uname = do
  Table tree <- ask
  return $ find tree $ User uname [] T.empty

$(makeAcidic ''UserTable ['insertUser', 'updateUrlsUser', 'queryUser', 'deleteUrlUser'])


------------ AUXILIARY FUNCTIONS ------------

-- | Checks if `User`'s username and hash are not empty
-- TODO: clarify if needed
unameHashNotNil :: User -> Bool
unameHashNotNil User{..} = username /= "" && hash /= ""

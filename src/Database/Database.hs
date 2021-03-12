module Database.Database
  ( createUser
  , getUser
  , getUserUrls
  , createUrl
  , Database.Database.deleteUrl
  , Database.Database.genUUID
  , openDB
  , closeDB
  , runDB
  ) where

import Data.Acid
import System.FilePath ((</>))
import Relude

import Database.Common
import Database.State
import Database.Url.Url
import Database.Url.UrlDB
import Database.User.User
import Database.User.UserDB
import qualified Database.Tree.Tree as BT

-- | Insert `User` in db
createUser :: DB m => User -> m ()
createUser = insertUser

-- | Searches for `User` with given `Username`
getUser :: DB m => Username -> m (Maybe User)
getUser = queryUser

-- | Searces for `ShortUrl`s associated with `User`
getUserUrls :: DB m => Username -> m (Maybe [ShortUrl])
getUserUrls = queryUserUrls

-- | Insert `Url` in db
createUrl :: DB m => Url -> Username -> m ()
createUrl = insertUrl

-- | Delete `Url` from db
deleteUrl :: DB m => ShortUrl -> Username -> m ()
deleteUrl = Database.Url.UrlDB.deleteUrl

-- | Generate new `UUID`
genUUID :: DB m => m UUID
genUUID = Database.Url.UrlDB.genUUID

-- | Open db state in given dir
openDB :: FilePath -> IO Tables
openDB dir = do
  userTable <- openLocalStateFrom (dir </> "userTable") (Table BT.empty      :: UserTable)
  urlTable  <- openLocalStateFrom (dir </> "urlTable")  (Table (BT.empty, 0) :: UrlTable)
  return (userTable, urlTable)

-- | Close db state
closeDB :: Tables -> IO ()
closeDB = (>>) . closeAcidState . fst <*> closeAcidState . snd

-- | Run db monad
runDB :: Tables -> AppDB a -> IO (Either DBError a)
runDB = (runExceptT .) . flip (runReaderT . runAppDB)

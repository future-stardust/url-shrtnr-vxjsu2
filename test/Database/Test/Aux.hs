module Database.Test.Aux
  ( run
  , right
  , left
  , runDB
  , cmp
  , itdb
  ) where

import Data.Acid
import Database.State
import Database.Common
import Database.User.User
import Database.User.UserDB
import Test.Hspec

import Control.Exception
import Control.Monad.Catch (MonadMask)
import Relude hiding (empty)
import Relude.Extra (bimapBoth)
import Data.RedBlackTree
import Database.Url.Url
import Data.Maybe (fromJust)

import System.IO.Temp
import System.FilePath ((</>))


run :: Tables -> AppDB a -> IO (Either DBError a)
run t a = runExceptT (runReaderT (runAppDB a) t)

right :: Exception e => Either e a -> IO a
right (Left  e) = throw e
right (Right v) = return v

left :: Show a => Either e a -> IO e
left (Left  e) = return e
left (Right v) = error $ "[error] expected 'Left e', but got: " <> show v

-- | Opens db state in given dir
initTables :: FilePath -> IO Tables
initTables dir = do
  -- TODO: implempent temporary dir for tests
  userTable <- openLocalStateFrom (dir </> "userTable") (Table empty      :: UserTable)
  urlTable  <- openLocalStateFrom (dir </> "urlTable")  (Table (empty, 0) :: UrlTable)
  return (userTable, urlTable)

-- | Opens connection, runs callback, closes connection
runDB :: FilePath -> (Tables -> IO ()) -> IO ()
runDB dir = bracket (initTables dir) $ \t -> closeAcidState (fst t)
                                          >> closeAcidState (snd t)

-- | Creates temporary dir for database
createTempDir :: (MonadMask m, MonadIO m) => (FilePath -> m a) -> m a
createTempDir runTables = do
  tmp <- liftIO getCanonicalTemporaryDirectory
  withTempDirectory tmp "shortener_test_tables" runTables

-- | Same as `it`, but injects in `it` opening and closing of database
itdb :: String -> (Tables -> IO ()) -> SpecWith (Arg (IO ()))
itdb name = it name . createTempDir . flip runDB

class Compare a where
  cmp :: a -> a -> Bool

-- TODO: use traverse with lenses or something instead
instance Compare User where
  cmp (User uname1 urls1 hash1) (User uname2 urls2 hash2) =
    uname1 == uname2 && urls1 == urls2 && hash1 == hash2


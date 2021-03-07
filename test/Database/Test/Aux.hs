module Database.Test.Aux
  ( right
  , left
  , itdb
  ) where

import Database.Database
import Database.State

import Control.Exception
import Control.Monad.Catch (MonadMask)
import Relude
import System.IO.Temp
import Test.Hspec


right :: Exception e => Either e a -> IO a
right (Left  e) = throw e
right (Right v) = return v

left :: Show a => Either e a -> IO e
left (Left  e) = return e
left (Right v) = error $ "[error] expected 'Left e', but got: " <> show v

-- | Opens connection, runs callback, closes connection
run :: FilePath -> (Tables -> IO ()) -> IO ()
run = flip bracket closeDB . openDB

-- | Creates temporary dir for database
createTempDir :: (MonadMask m, MonadIO m) => (FilePath -> m a) -> m a
createTempDir runTables = do
  tmp <- liftIO getCanonicalTemporaryDirectory
  withTempDirectory tmp "shortener_test_tables" runTables

-- | Same as `it`, but injects in `it` opening and closing of database
itdb :: String -> (Tables -> IO ()) -> SpecWith (Arg (IO ()))
itdb name = it name . createTempDir . flip run

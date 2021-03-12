module Server.Handler.UsersSpec (spec) where

import           Control.Exception
import           Network.HTTP.Client      hiding (Proxy)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import           Servant.Client
import           Server.Server
import           Server.Types             as T

import           System.Directory
import           System.IO.Temp

import           Test.Hspec

import           Database.Database        as D
import           Relude
import           Server.API.Users

withUserApp :: FilePath -> (Warp.Port -> IO ()) -> IO ()
withUserApp db action = do
  bracket (D.openDB db)
    D.closeDB
    (\tbs -> do
      Warp.testWithApplication (pure . app $ AppCtx tbs) action)

spec :: Spec
spec = do
  tmp <- runIO getCanonicalTemporaryDirectory
  tmp_db <- runIO $ createTempDirectory tmp "test_db"

  around (withUserApp tmp_db) $ do

    let signup :<|> signin :<|> signout = client (Proxy :: Proxy Users)

    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager baseUrl { baseUrlPort = port }

    describe "/users" $ do
      it "/signup" $ \port -> do
        let usr = T.User "test@test.com" "testpasswd"
        got <- runClientM (signup usr) $ clientEnv port
        got `shouldBe` Right NoContent

      it "/signin" $ \port -> do
        let usr = T.User "test@test.com" "testpasswd"
        got <- runClientM (signin usr) $ clientEnv port
        got `shouldBe` Right (Token "test@test.com\ntestpasswd")

      it "/signout" $ \port -> do
        got <- runClientM (signout) $ clientEnv port
        got `shouldBe` Right NoContent

  runIO $ removeDirectoryRecursive tmp_db

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handler.UsersSpec (spec) where

import           Control.Exception
import           Network.HTTP.Client      hiding (Proxy)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import           Servant.Client
import           Server.Server
import           Server.Types             as T

import           Colog

import           System.Directory
import           System.IO.Temp

import           Test.Hspec

import           Relude

import           Database.Database        as D
import           Server.API.Users

withUserApp :: FilePath -> (Warp.Port -> IO ()) -> IO ()
withUserApp db action = do
  bracket (D.openDB db)
    D.closeDB
    (\tbs -> do
      let appCtx = AppCtx tbs (LogAction . const $ return ())
      Warp.testWithApplication (app appCtx) action)

spec :: Spec
spec = do
  tmp <- runIO getCanonicalTemporaryDirectory
  tmp_db <- runIO $ createTempDirectory tmp "test_db"

  around (withUserApp tmp_db) $ do

    let api :: Proxy (SignUp :<|> SignIn :<|> BasicAuth "test" T.User :> SignOut)
        api = Proxy

    let signup :<|> signin :<|> signout = client api

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
        getHeaders <$> got `shouldSatisfy` isRight

      it "/signout" $ \port -> do
        got <- runClientM (signout (BasicAuthData "test@test.com" "testpasswd")) $ clientEnv port
        getHeaders <$> got `shouldSatisfy` isRight

  runIO $ removeDirectoryRecursive tmp_db

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handler.UrlsSpec (spec) where

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

import qualified Database.Database        as D
import           Server.API.Urls

withUserApp :: FilePath -> (Warp.Port -> IO ()) -> IO ()
withUserApp db action = do
  bracket (D.openDB db)
    D.closeDB
    (\tbs -> do
      let usr = D.User "test@test.com" [] "testpasswd"
          appCtx = AppCtx tbs (LogAction . const $ return ())
      void . D.runDB tbs $ D.createUser usr -- create test user before running tests
      Warp.testWithApplication (app appCtx) action)

spec :: Spec
spec = do
  tmp <- runIO getCanonicalTemporaryDirectory
  tmp_db <- runIO $ createTempDirectory tmp "test_db"

  around (withUserApp tmp_db) $ do

    let api :: Proxy (BasicAuth "test" T.User :> Urls)
        api = Proxy
        shortenUrl :<|> listUrls :<|> deleteUrl = client api (BasicAuthData "test@test.com" "testpasswd")

    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager baseUrl { baseUrlPort = port }

    describe "/urls/shorten" $ do
      it "Without specified alias" $ \port -> do
        let srb = ShortenReqBody "https://kek.lol/arbidol1" Nothing
            expected = ShortenedUrl "1" -- we have only one user in database right now

        got <- runClientM (shortenUrl srb) $ clientEnv port
        got `shouldBe` Right expected

      it "With specified alias" $ \port -> do
        let srb = ShortenReqBody "https://kek.lol/arbidol2" $ Just "test_alias"
            expected = ShortenedUrl "test_alias"

        got <- runClientM (shortenUrl srb) $ clientEnv port
        got `shouldBe` Right expected

    describe "list urls" $ do
      it "After creation of 2 urls" $ \port -> do
        got <- runClientM listUrls $ clientEnv port
        got `shouldBe` Right ["1", "test_alias"] -- we created 2 urls with previous test

    describe "delete url" $ do
      it "Delete created earlier url" $ \port -> do
        got <- runClientM (deleteUrl "test_alias") $ clientEnv port
        got `shouldBe` Right NoContent

    describe "list urls" $ do
      it "After deletion" $ \port -> do
        got <- runClientM listUrls $ clientEnv port
        got `shouldBe` Right ["1"] -- cleaned up "test_alias" url

  runIO $ removeDirectoryRecursive tmp_db

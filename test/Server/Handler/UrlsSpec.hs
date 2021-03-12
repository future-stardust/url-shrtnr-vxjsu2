module Server.Handler.UrlsSpec (spec) where

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

import           Relude

import qualified Database.Database        as D
import           Server.API.Urls

withUserApp :: FilePath -> (Warp.Port -> IO ()) -> IO ()
withUserApp db action = do
  bracket (D.openDB db)
    D.closeDB
    (\tbs -> do
      let usr = D.User "test@test.com" [] "testpasswd"
      void . D.runDB tbs $ D.createUser usr -- create test user before running tests
      Warp.testWithApplication (pure . app $ AppCtx tbs) action)

spec :: Spec
spec = do
  tmp <- runIO getCanonicalTemporaryDirectory
  tmp_db <- runIO $ createTempDirectory tmp "test_db"

  around (withUserApp tmp_db) $ do

    let shortenUrl :<|> listUrls :<|> deleteUrl = client (Proxy :: Proxy Urls)

    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager baseUrl { baseUrlPort = port }
        token = Just $ Token "test@test.com\ntestpasswd"

    describe "/urls/shorten" $ do
      it "Without specified alias" $ \port -> do
        let srb = ShortenReqBody "https://kek.lol/arbidol1" Nothing
            expected = ShortenedUrl "1" -- we have only one user in database right now
        got <- runClientM (shortenUrl srb token) $ clientEnv port
        got `shouldBe` Right expected

      it "With specified alias" $ \port -> do
        let srb = ShortenReqBody "https://kek.lol/arbidol2" $ Just "test_alias"
            expected = ShortenedUrl "test_alias"
        got <- runClientM (shortenUrl srb token) $ clientEnv port
        got `shouldBe` Right expected

      it "Without token" $ \port -> do
        let srb = ShortenReqBody "https://kek.lol/arbidol3" Nothing
            token = Nothing
        got <- runClientM (shortenUrl srb token) $ clientEnv port
        got `shouldSatisfy` isLeft

    describe "list urls" $ do
      it "With token" $ \port -> do
        got <- runClientM (listUrls token) $ clientEnv port
        got `shouldBe` Right ["1", "test_alias"] -- we created 2 urls with previous test

      it "Without token" $ \port -> do
        let token = Nothing
        got <- runClientM (listUrls token) $ clientEnv port
        got `shouldSatisfy` isLeft

    describe "delete url" $ do
      it "With token" $ \port -> do
        got <- runClientM (deleteUrl "test_alias" token) $ clientEnv port
        got `shouldBe` Right NoContent

      it "Without token" $ \port -> do
        let token = Nothing
        got <- runClientM (deleteUrl "test_alias" token) $ clientEnv port
        got `shouldSatisfy` isLeft

    describe "list urls" $ do
      it "After deletion" $ \port -> do
        got <- runClientM (listUrls token) $ clientEnv port
        got `shouldBe` Right ["1"] -- cleaned up "test_alias" url

  runIO $ removeDirectoryRecursive tmp_db

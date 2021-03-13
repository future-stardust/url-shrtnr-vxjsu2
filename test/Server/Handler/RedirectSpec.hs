module Server.Handler.RedirectSpec (spec) where

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

import           Database.Database        as D
import           Network.HTTP.Types
import           Server.API.Redirect

withUserApp :: FilePath -> (Warp.Port -> IO ()) -> IO ()
withUserApp db action = do
  bracket (D.openDB db)
    D.closeDB
    (\tbs -> do
      let usr = D.User "test@test.com" [] "testpasswd"
          url = D.Url "original_test" "test_alias"
      void . D.runDB tbs $ D.createUser usr -- create test user before running tests
      void . D.runDB tbs $ D.createUrl url "test@test.com" -- create test url
      Warp.testWithApplication (app $ AppCtx tbs) action)

spec :: Spec
spec = do
  tmp <- runIO getCanonicalTemporaryDirectory
  tmp_db <- runIO $ createTempDirectory tmp "test_db"

  around (withUserApp tmp_db) $ do

    let redirect = client (Proxy :: Proxy Redirect)

    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager baseUrl { baseUrlPort = port }

    describe "/r" $ do
      it "/test_alias" $ \port -> do
        got <- runClientM (redirect "test_alias") $ clientEnv port

        -- We expect redirect
        let expected (Left (FailureResponse _ Response{..})) =
              statusCode responseStatusCode == 302 -- 302 status code
              && any (\(hn, hb) -> hn == "Location" && hb == "original_test") responseHeaders -- and Location with "original_test"
            expected _ = False

        got `shouldSatisfy` expected

  runIO $ removeDirectoryRecursive tmp_db

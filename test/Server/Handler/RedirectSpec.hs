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

import           Database.Database        as D
import           Relude
import           Server.API.Redirect
import Network.HTTP.Types

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

    let redirect = client (Proxy :: Proxy Redirect)

    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager baseUrl { baseUrlPort = port }

    describe "/r" $ do
      it "/test_alias" $ \port -> do
        got <- runClientM (redirect "test_alias") $ clientEnv port
        let expected (Left (FailureResponse _ Response{..})) =
              statusCode responseStatusCode == 302
              && any (\(hn, hb) -> hn == "Location" && hb == "original_test") responseHeaders
            expected _ = False
        got `shouldSatisfy` expected

  runIO $ removeDirectoryRecursive tmp_db

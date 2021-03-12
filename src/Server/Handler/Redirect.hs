module Server.Handler.Redirect
  ( redirectH
  )
where

import           Servant

import           Database.Database
import           Relude
import           Server.Types
import           Server.Types.Util

redirectH :: Text -> HandlerT NoContent
redirectH alias = do
  tbs <- asks tables

  res <- liftIO . runDB tbs $ queryUrl alias

  case res of
    Right (Just orig) -> throwError err302 { errBody = encodeUtf8 orig }
    Right Nothing     -> throwError err404 { errBody = "Alias not found" }
    Left e            -> throwError $ dbToServerError e

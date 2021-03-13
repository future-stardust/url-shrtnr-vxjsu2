module Server.Handler.Redirect
  ( redirectH
  )
where

import           Servant

import           Database.Database
import           Relude
import           Server.Types
import           Server.Types.Util
import           Server.Util       (logWith)

redirectH :: Text -> HandlerT NoContent
redirectH alias = do
  log <- asks logger
  logWith log $ "Redirecting by alias " <> alias
  tbs <- asks tables
  res <- liftIO . runDB tbs $ queryUrl alias
  case res of
    Right (Just orig) -> do
      logWith log $ "Successful redirect, origin: " <> orig
      throwError err302 { errHeaders = [("Location", encodeUtf8 orig)] }
    Right Nothing     -> do
      logWith log "Alias not found"
      throwError err404 { errBody = "Alias not found" }
    Left e            -> do
      logWith log $ "Got error: " <> show e
      throwError $ dbToServerError e

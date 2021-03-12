module Server.Handler.Redirect
  ( redirectH
  )
where

import           Servant

import           Database.Database
import           Relude
import           Server.Types
import           Server.Types.Util

redirectH :: Text -> Maybe Token -> HandlerT NoContent
redirectH _     Nothing = throwError err404 { errBody = "Can't find cookies" }
redirectH alias (Just t) = do
  let d = tokenToData t
  name <- case d of
            Just (n, _) -> return n
            Nothing -> throwError err500 { errBody = "Can't decode cookies" }
  tbs <- asks tables

  res <- liftIO . runDB tbs $ undefined

  case res of
    Right orig -> throwError err302 { errBody = orig }
    Left e     -> throwError $ dbToServerError e

{-# LANGUAGE DataKinds     #-}

{-# LANGUAGE TypeOperators #-}
module Server.Auth
  ( lookupUser
  , authHandler
  , authContext
  )
where

import           Network.Wai                      (Request (requestHeaders))
import           Servant
import           Web.Cookie

import           Relude

import           Data.List                        (lookup)
import           Database.Database
import           Database.User.User               as U
import           Servant.Server.Experimental.Auth
import           Server.Types                     as T
import           Server.Types.Util                (dbToServerError)


lookupUser :: Text -> HandlerT T.User
lookupUser token = do
  tbs <- asks tables
  let [name, passw] = lines token
  user <- liftIO . runDB tbs $ getUser name
  case user of
    Right (Just u) -> if U.hash u == passw
                      then return $ T.User (U.username u) (U.hash u)
                      else throwError err403 { errBody = "Invalid password" }
    Right Nothing  -> throwError err404 { errBody = "User not found" }
    Left  e        -> throwError $ dbToServerError e


authHandler :: AppCtx -> AuthHandler Request T.User
authHandler ctx = mkAuthHandler (flip runReaderT ctx . handler)
  where
    maybeToEither e = maybe (Left e) Right
    throw401 msg = throwError $ err401 { errBody = msg }
    handler req = either throw401 (lookupUser . decodeUtf8) $ do
      cookie <- maybeToEither "Missing cookie header"
                . lookup "cookie"
                $ requestHeaders req
      maybeToEither "Missing token in cookie"
        . lookup "servant-auth-cookie"
        $ parseCookies cookie

authContext :: AppCtx -> Context (AuthHandler Request T.User ': '[])
authContext ctx = authHandler ctx :. EmptyContext

type instance AuthServerData (AuthProtect "cookie-auth") = T.User

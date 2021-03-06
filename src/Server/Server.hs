{-# LANGUAGE DataKinds #-}

module Server.Server
  ( server
  , app
  , up
  )
where

import           Network.Wai.Handler.Warp
import           Servant

import           Relude

import           Servant.Auth.Server
import           Server.API
import           Server.Auth
import           Server.Handler
import           Server.Types


serverT :: JWTSettings -> CookieSettings -> ServerT API HandlerT
serverT jwts cks = signUpH
  :<|> signInH cks jwts
  :<|> signOutH cks
  :<|> shortenH
  :<|> listUrlsH
  :<|> deleteUrlH
  :<|> redirectH

context :: Proxy '[JWTSettings, CookieSettings, BasicAuthCfg]
context = Proxy

server :: AppCtx -> JWTSettings -> CookieSettings -> Server API
server ctx jwts cks = hoistServerWithContext api context (toH ctx) $ serverT jwts cks

-- | WAI Application
app :: AppCtx -> IO Application
app ctx = do
  key <- generateKey
  let jwtCfg = defaultJWTSettings key
      authCfg = authCheck $ tables ctx
      cookieCfg = defaultCookieSettings
      cfg = jwtCfg :. cookieCfg :. authCfg :. EmptyContext
  return . serveWithContext api cfg $ server ctx jwtCfg cookieCfg

-- | Start up the service
up :: Port -> AppCtx -> IO ()
up = (<=< app) . run
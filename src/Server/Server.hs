module Server.Server
  ( server
  , app
  )
where

import           Servant

import           Relude

import           Servant.API.Flatten
import           Server.API
import           Server.Handler
import           Server.Types


serverT :: ServerT (Flat API) HandlerT
serverT = signUpH :<|> signInH :<|> signOutH
          :<|> shortenH :<|> listUrlsH :<|> deleteUrlH
          :<|> redirectH

server :: AppCtx -> Server (Flat API)
server ctx = hoistServer (flatten api) (toH ctx) serverT

app :: AppCtx -> Application
app ctx = serve (flatten api) $ server ctx

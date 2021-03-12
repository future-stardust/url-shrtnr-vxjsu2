module Server.Server
  ( server
  , app
  , up
  )
where

import           Servant

import           Relude

import           Server.API
import           Server.Handler
import           Server.Types
import Network.Wai.Handler.Warp (run)


serverT :: ServerT API HandlerT
serverT = signUpH :<|> signInH :<|> signOutH
          :<|> shortenH :<|> listUrlsH :<|> deleteUrlH
          :<|> redirectH

server :: AppCtx -> Server API
server ctx = hoistServer api (toH ctx) serverT

app :: AppCtx -> Application
app ctx = serve api $ server ctx

up :: AppCtx -> IO ()
up ctx = run 8080 $ app ctx

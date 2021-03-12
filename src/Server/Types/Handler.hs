module Server.Types.Handler
  ( HandlerT
  , toH
  )
where

import           Servant
import           Server.Types.AppCtx

import           Relude

-- | Custom Handler monad
type HandlerT = ReaderT AppCtx Handler

toH :: AppCtx -> HandlerT b -> Handler b
toH = flip runReaderT

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.API
  ( API
  , api
  )
where

import           Servant.API
import           Server.API.Redirect
import           Server.API.Urls
import           Server.API.Users

import           Relude
import Servant.API.Flatten

type API = Flat (Users :<|> Urls :<|> Redirect)

api :: Proxy API
api = Proxy

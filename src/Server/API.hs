{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.API
  ( API
  , api
  )
where

import           Servant.API
import           Servant.API.Flatten
import qualified Servant.Auth        as SA

import           Relude

import           Server.API.Redirect
import           Server.API.Urls
import           Server.API.Users
import           Server.Types

-- | Service API
type API = Flat (Users :<|> SA.Auth '[SA.JWT, SA.BasicAuth] User :> Urls :<|> Redirect)

api :: Proxy API
api = Proxy

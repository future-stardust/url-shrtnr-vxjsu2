{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.API
  ( API
  )
where

import           Servant.API

import           Server.API.Redirect
import           Server.API.Urls
import           Server.API.Users

type API = Users :<|> Urls :<|> Redirect


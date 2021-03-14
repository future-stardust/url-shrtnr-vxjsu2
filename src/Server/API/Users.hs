{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.API.Users
  ( Users
  , SignUp
  , SignIn
  , SignOut
  )
where

import qualified Servant.Auth as SA

import           Servant.API
import           Server.Auth
import           Server.Types

type SignUp = "users" :> "signup" :> ReqBody '[JSON] User :> Post '[JSON] NoContent

type SignIn = "users" :> "signin" :> ReqBody '[JSON] User :> Post '[JSON] AuthNoContent

type SignOut = "users" :> "signout" :> Verb 'POST 204 '[JSON] AuthNoContent


type Users = SignUp :<|> SignIn :<|> SA.Auth '[SA.JWT, SA.BasicAuth] User :> SignOut

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.API.Users
  ( Users
  , SignUp
  , SignIn
  , SignOut
  )
where

import qualified Servant.Auth        as SA
import           Servant.Auth.Server

import           Servant.API
import           Server.Types

type SignUp = "users" :> "signup" :> ReqBody '[JSON] User :> Post '[JSON] NoContent

type SignIn = "users" :> "signin" :> ReqBody '[JSON] User :>
  Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

type SignOut = "users" :> "signout" :>
  Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)


type Users = SignUp :<|> SignIn :<|> SA.Auth '[SA.JWT, SA.BasicAuth] User :> SignOut

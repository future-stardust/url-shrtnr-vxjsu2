{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.API.Users
  ( Users
  )
where

import           Servant.API
import           Server.Types

type SignUp = "users" :> "signup" :> ReqBody '[JSON] User :> Post '[JSON] NoContent

type SignIn = "users" :> "signin" :> ReqBody '[JSON] User :> Post '[JSON] Token

type SignOut = "users" :> "signout" :> Post '[JSON] NoContent


type Users = SignUp :<|> SignIn :<|> SignOut

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.API.Users
  ( Users
  )
where

import           Servant.API
import           Server.Types

type SignUp = "users" :> "signup" :> ReqBody '[JSON] User :> Post '[] NoContent

type SignIn = "users" :> "signin" :> ReqBody '[JSON] User :> Post '[JSON] Token

type SignOut = "users" :> "signout" :> Post '[] NoContent


type Users = SignUp :<|> SignIn :<|> SignOut

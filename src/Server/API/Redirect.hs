{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.API.Redirect
  ( Redirect
  )
where

import           Relude
import           Servant.API

type Redirect = "r" :> Capture "alias" Text :> Get '[JSON] NoContent

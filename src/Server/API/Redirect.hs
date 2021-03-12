{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.API.Redirect
  ( Redirect
  )
where

import           Relude
import           Servant.API
import           Server.Types

type Redirect = "r" :> Capture "alias" Text :> Header "Cookie" Token :> Get '[JSON] NoContent

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.API.Redirect
  ( Redirect
  )
where

import           Data.Text   (Text)
import           Servant.API

type Redirect = "r" :> Capture "alias" Text :> Get '[] NoContent

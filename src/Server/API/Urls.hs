{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.API.Urls
  ( Urls
  )
where

import           Data.Text    (Text)
import           Servant.API
import           Server.Types

type Shorten = "urls" :> "shorten" :> ReqBody '[JSON] ShortenReqBody :> Post '[JSON] ShortenedUrl

type ListUrls = "urls" :> Get '[JSON] [Url]

type DeleteUrl = "urls" :> Capture "alias" Text :> Delete '[] NoContent


type Urls = Shorten :<|> ListUrls :<|> DeleteUrl

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.API.Urls
  ( Urls
  )
where

import           Data.Text       (Text)
import           Database.Common
import           Servant.API
import           Server.Types

type Shorten = "urls" :> "shorten" :> ReqBody '[JSON] ShortenReqBody :>
  Header "Cookie" Token :> Post '[JSON] ShortenedUrl

type ListUrls = "urls" :> Header "Cookie" Token :> Get '[JSON] [ShortUrl]

type DeleteUrl = "urls" :> Capture "alias" Text :> Header "Cookie" Token :> Delete '[JSON] NoContent


type Urls = Shorten :<|> ListUrls :<|> DeleteUrl

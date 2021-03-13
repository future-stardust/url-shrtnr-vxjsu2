{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.API.Urls
  ( Urls
  , Shorten
  , ListUrls
  , DeleteUrl
  )
where

import           Data.Text       (Text)
import           Servant.API

import           Database.Common
import           Server.Types

type Shorten = "urls" :> "shorten" :> ReqBody '[JSON] ShortenReqBody :> Post '[JSON] ShortenedUrl

type ListUrls = "urls" :> Get '[JSON] [ShortUrl]

type DeleteUrl = "urls" :> Capture "alias" Text :> Delete '[JSON] NoContent


type Urls = (Shorten :<|> ListUrls :<|> DeleteUrl)

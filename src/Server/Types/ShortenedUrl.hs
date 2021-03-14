{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}

module Server.Types.ShortenedUrl where

import           Data.Aeson          hiding (Encoding)
import           Data.Aeson.Deriving
import           GHC.Generics
import           Server.Types.Util

import           Relude

-- | Internal type used in "urls/" route. Represents shortened url.
newtype ShortenedUrl = ShortenedUrl
  { suShortenedUrl :: Text -- ^ Shortened url
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (Encoding ShortenedUrl)

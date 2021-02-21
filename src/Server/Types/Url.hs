{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}

module Server.Types.Url where

import           Data.Aeson          hiding (Encoding)
import           Data.Aeson.Deriving
import           GHC.Generics
import           Server.Types.Util

import Relude

-- | Internal type used in "urls/" route. Represents url.
data Url = Url
  { uUrl           :: Text -- ^ Original url
  , uAlias        :: Text -- ^ Alias for url
  , uShortenedUrl :: Text -- ^ Shortened url
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (Encoding Url)

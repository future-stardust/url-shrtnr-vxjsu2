{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}

module Server.Types.ShortenReqBody where

import           Data.Aeson          hiding (Encoding)
import           Data.Aeson.Deriving
import           GHC.Generics
import           Server.Types.Util

import           Relude

-- | Internal type used in "urls/" route. Represents shortened endpoint body.
data ShortenReqBody = ShortenReqBody
  { srbUrl   :: Text -- ^ Original url
  , srbAlias :: Maybe Text -- ^ Alias for url
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (Encoding ShortenReqBody)


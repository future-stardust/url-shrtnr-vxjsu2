{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}

module Server.Types.Token where

import           Data.Aeson          hiding (Encoding)
import           Data.Aeson.Deriving
import           GHC.Generics
import           Server.Types.Util

import Relude

-- | Internal type used in "users/" route. Represents token.
data Token = Token
  { tToken :: Text -- ^ The token itself
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (Encoding Token)

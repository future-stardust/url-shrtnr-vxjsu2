{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}

module Server.Types.User where

import           Data.Aeson          hiding (Encoding)
import           Data.Aeson.Deriving
import           GHC.Generics
import           Servant.Auth.JWT

import           Relude

import           Server.Types.Util

-- | Internal type used in "users/" route and auth stuff. Represents user.
data User = User
  { userEmail    :: Text -- ^ User email
  , userPassword :: Text -- ^ User password
  }
  deriving (Eq, Show, Generic, ToJWT, FromJWT)
  deriving (FromJSON, ToJSON) via (Encoding User)

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}

module Server.Types.Token where

import           Data.Aeson          hiding (Encoding)
import           Data.Aeson.Deriving
import           GHC.Generics
import           Server.Types.Util

import           Relude
import           Servant

-- | Internal type used in "users/" route. Represents token.
data Token = Token
  { tToken :: Text -- ^ The token itself
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (Encoding Token)

tokenToData :: Token -> Maybe (Text, Text)
tokenToData Token{..} =
  let t = lines tToken
  in if length t == 2
     then let [name, passw] = t in Just (name, passw)
     else Nothing

instance ToHttpApiData Token where
  toUrlPiece = decodeUtf8 . encode

instance FromHttpApiData Token where
  parseUrlPiece = bimap toText identity . eitherDecode . encodeUtf8

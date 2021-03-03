{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}


module Database.Common
  ( Table (..)
  , UUID
  , Username
  , Hash
  , OrigUrl
  , ShortUrl
  , DBError(..)
  , shortUrlIsNil
  ) where

import Data.SafeCopy
import qualified Data.Text as T
import Relude hiding (empty, find)
import qualified Text.Show


-- | Seed, used for generating unique shortened urls
type UUID     = Int
-- TODO: use NonEmpty for `Username`, `OrigUrl`
-- and `ShortUrl` instead (?)
-- | `User`'s username
type Username = T.Text
-- | `User`'s hashed password
type Hash     = T.Text
-- | Original url
type OrigUrl  = T.Text
-- | Shortened url
type ShortUrl = T.Text


-- | Database error
data DBError where
  EUserExist  :: HasCallStack => DBError  -- ^ user with such username already exists
  EUserNExist :: HasCallStack => DBError  -- ^ username doesn't exist in db
  EUserNil    :: HasCallStack => DBError  -- ^ username or hash is empty
  EUrlNil     :: HasCallStack => DBError  -- ^ shortened or original url are empty
  EUrlExist   :: HasCallStack => DBError  -- ^ url already exists
  EUrlNExist  :: HasCallStack => DBError  -- ^ url doesn't exist in db
  ESUrlNil    :: HasCallStack => DBError  -- ^ value of shortened url is nil
  ESUrlExist  :: HasCallStack => DBError  -- ^ shortened url already exists
  ESUrlNExist :: HasCallStack => DBError  -- ^ shortened url doesn't exist

instance Show DBError where
  show EUserExist  = "Such user already exist\n"  <> prettyCallStack callStack
  show EUserNExist = "Such user doesn't exists\n" <> prettyCallStack callStack
  show EUserNil    = "Username or hash of user are empty\n" <> prettyCallStack callStack
  show EUrlNil     = "Shortened or original url are empty\n" <> prettyCallStack callStack
  show EUrlExist   = "Such url already exists\n"  <> prettyCallStack callStack
  show EUrlNExist  = "Such url doesn't exist\n"   <> prettyCallStack callStack
  show ESUrlNil    = "Short url value is empty\n" <> prettyCallStack callStack
  show ESUrlExist  = "Such shortened url already exists\n" <> prettyCallStack callStack
  show ESUrlNExist = "Such shortened url doesn't exist\n"  <> prettyCallStack callStack


-- TODO: maybe there's a better alternative
-- how to derive Eq for DBError
instance Eq DBError where
  (==) EUserExist  EUserExist  = True
  (==) EUserNExist EUserNExist = True
  (==) EUserNil    EUserNil    = True
  (==) EUrlNil     EUrlNil     = True
  (==) EUrlExist   EUrlExist   = True
  (==) EUrlNExist  EUrlNExist  = True
  (==) ESUrlNil    ESUrlNil    = True
  (==) ESUrlExist  ESUrlExist  = True
  (==) ESUrlNExist ESUrlNExist = True
  (==) _ _ = False

deriving anyclass instance Exception DBError

-- | Used for representing logical unit of db
newtype Table a = Table { unTable :: a } deriving (Functor, Show)

$(deriveSafeCopy 0 'base ''Table)

-- | Checks if shortened url is not empty
shortUrlIsNil :: ShortUrl -> Bool
shortUrlIsNil = (==) ""

{-# LANGUAGE ConstraintKinds #-}

module Database.State
  ( AppDB(..)
  , Tables
  , DB
  ) where

import Control.Monad.Except
import Data.Acid
import Database.Common
import Database.Url.Url
import Database.User.User

import Relude

-- | Logical separation of different data structs in db
type Tables = (AcidState UserTable, AcidState UrlTable)

-- | Representation of DB
type DB m = (MonadIO m, MonadError DBError m, MonadReader Tables m)

newtype AppDB a = AppDB
  { runAppDB :: ReaderT Tables (ExceptT DBError IO) a
  } deriving ( Monad
             , Functor
             , Applicative
             , MonadReader Tables
             , MonadError DBError
             , MonadIO)

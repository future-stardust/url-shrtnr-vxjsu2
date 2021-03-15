module Server.Types.AppCtx
  ( AppCtx(..)
  )
where

import           Colog

import           Relude

import           Database.State (Tables)

-- | Context for Handler's used in ReaderT as configuration
data AppCtx = AppCtx
  { tables :: Tables
  , logger :: LogAction IO Text
  }

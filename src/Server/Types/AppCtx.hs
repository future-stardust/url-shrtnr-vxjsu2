module Server.Types.AppCtx
  ( AppCtx(..)
  )
where

import           Database.State (Tables)

-- | Context for Handler's
data AppCtx = AppCtx
  { tables :: Tables
  }

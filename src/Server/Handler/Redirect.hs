module Server.Handler.Redirect
  ( redirectH
  )
where

import           Servant

import           Relude
import           Server.Types

redirectH :: Text -> HandlerT NoContent
redirectH alias = undefined

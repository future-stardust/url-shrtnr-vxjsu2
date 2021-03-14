{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.Types.Util where

import           Data.Aeson.Deriving
import           Database.Common     (DBError (..))
import           Servant

type Encoding = GenericEncoded
  '[ FieldLabelModifier := [ SnakeCase, DropLowercasePrefix  ]
   ]

dbToServerError :: DBError -> ServerError
dbToServerError = \case
    EUserExist  -> err409 { errBody = "User with such email already exists" }
    EUserNExist -> err404 { errBody = "User with such email doesn't exist"  }
    EUserNil    -> err406 { errBody = "Email or passw is empty"             }
    EUrlNil     -> err406 { errBody = "Shortened or original url are empty" }
    EUrlExist   -> err409 { errBody = "Url already exists"                  }
    EUrlNExist  -> err404 { errBody = "Url doesn't found"                   }
    ESUrlNil    -> err500 { errBody = "Shortened url is null"               }
    ESUrlExist  -> err500 { errBody = "Shortened url already exists"        }
    ESUrlNExist -> err500 { errBody = "Shortened url doesn't exist"         }

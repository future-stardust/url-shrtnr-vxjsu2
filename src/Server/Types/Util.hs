{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.Types.Util where

import           Data.Aeson.Deriving
import           Database.Common     (DBError (..))
import           Servant

-- | Helper alias to derive JSON instances for some types using deriving via
type Encoding = GenericEncoded
  '[ FieldLabelModifier := [ SnakeCase, DropLowercasePrefix  ]
   ]

-- | Helper function to easily convert from DBError to ServerError
dbToServerError :: DBError -> ServerError
dbToServerError dbe = case dbe of
                        EUserExist  -> err409 { errBody = "User with such email already exists" }
                        EUserNExist -> err404 { errBody = "User with such email doesn't exist" }
                        EUserNil    -> err406 { errBody = "Email or passw is empty" }
                        EUrlNil     -> err406 { errBody = "Shortened or original url are empty" }
                        EUrlExist   -> err409 { errBody = "Url already exists" }
                        EUrlNExist  -> err404 { errBody = "Url is not found" }
                        ESUrlNil    -> err500 { errBody = "Shortened url is null" }
                        ESUrlExist  -> err500 { errBody = "Shortened url already exists" }
                        ESUrlNExist -> err500 { errBody = "Shortened url doesn't exist" }

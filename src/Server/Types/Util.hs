{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.Types.Util where

import           Data.Aeson.Deriving

type Encoding = GenericEncoded
  '[ FieldLabelModifier := [ SnakeCase, DropLowercasePrefix  ]
   ]

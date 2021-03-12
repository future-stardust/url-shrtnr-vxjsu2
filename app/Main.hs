module Main where

import Relude

import Server.Server
import Server.Types
import Database.Database
import Control.Exception

main :: IO ()
main = do
  putTextLn "kek" -- yes, this must be here at all costs
  bracket (openDB "database")
    closeDB
    (up . AppCtx)


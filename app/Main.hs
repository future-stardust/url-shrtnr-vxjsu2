{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Colog

import           Relude

import           Control.Exception
import           Database.Database
import           Server.Server
import           Server.Types

main :: IO ()
main = do
  let logger = LogAction putTextLn
  putTextLn "kek" -- yes, this must be here at all costs
  bracket (openDB "database")
    closeDB
    (up . flip AppCtx logger)


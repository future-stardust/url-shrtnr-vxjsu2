{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import           Colog
import           Control.Exception

import           Relude

import           Database.Database
import           Server.Server
import           Server.Types

main :: IO ()
main = do
  putTextLn "kek" -- yes, this must be here at all costs

  let logger = LogAction putTextLn
      port   = 8080

  putTextLn   "            _nnnn_"
  putTextLn $ "           dGGGGMMb     ," <> toText (['\"'| _ <- [0..34]]) <> "."
  putTextLn $ "          @p~qp~~qMb    | Running at http://localhost:" <> show port <> "/ | "
  putTextLn   "          M|@||@) M|   _;...................................'"
  putTextLn   "          @,----.JM| -'"
  putTextLn   "         JS^\\__/  qKL"
  putTextLn   "        dZP        qKRb"
  putTextLn   "       dZP          qKKb"
  putTextLn   "      fZP            SMMb"
  putTextLn   "      HZM            MMMM"
  putTextLn   "      FqM            MMMM"
  putTextLn   "    __| \".        |\\dS\"qML"
  putTextLn   "   |    `.       | `' \\Zq"
  putTextLn   "  _)      \\.___.,|     .'"
  putTextLn   "  \\____   )MMMMMM|   .'"
  putTextLn   "       `-'       `--'\""

  bracket (openDB "database")
    closeDB
    (up port . flip AppCtx logger)


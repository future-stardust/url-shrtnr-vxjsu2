module Main where

import qualified Data.Text as T
import           Data.RedBlackTree
import           Data.Acid

import           Data.SafeCopy
import           System.Environment   (getArgs)
import Data.Maybe
import Relude hiding (empty, find)


main :: IO ()
main = putTextLn "kek"
  -- args <- getArgs
  -- database <- openLocalStateFrom "myDatabase/" (Database $ singleton
  --                                               $ Url "kek" "kek" $ User "lol" "lol" )
  -- if null args
  --   then do messages <- query database (QueryDB urlEmpty)
  --           putStrLn "Last 10 messages:"
  --           print messages
  --   else do update database (InsertDB $ Url "First" "Second" $ User "Uname" "mail")
  --           putStrLn "Your message has been added to the database."

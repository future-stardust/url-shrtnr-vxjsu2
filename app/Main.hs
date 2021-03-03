module Main where

import Relude

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

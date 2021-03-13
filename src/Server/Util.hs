module Server.Util where

import           Colog              (LogAction, (<&))
import           Data.List.NonEmpty ((!!))
import           Relude

alphabet :: NonEmpty Char
alphabet = '0' :| (['1'..'9'] <> ['a'..'z'] <> ['A'..'Z'])

shortenWithAlphabet :: NonEmpty Char -> Int -> Text
shortenWithAlphabet a 0 = toText [head a]
shortenWithAlphabet a i = rest <> toText [digit]
  where
    base = length alphabet
    digit = a !! (i `mod` base)
    remainder = i `div` base
    rest = if remainder > 0
           then shortenWithAlphabet a remainder
           else ""

logWith :: MonadIO m => LogAction IO Text -> Text -> m ()
logWith log msg = void . liftIO $ log <& msg

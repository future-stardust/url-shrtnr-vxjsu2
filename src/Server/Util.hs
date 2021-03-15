module Server.Util where

import           Colog              (LogAction, (<&))
import           Data.List.NonEmpty ((!!))
import           Relude

-- | Standard base 62 alphabet
alphabet :: NonEmpty Char
alphabet = '0' :| (['1'..'9'] <> ['a'..'z'] <> ['A'..'Z'])

-- | Convert string using the NonEmpty Char as alphabet
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

-- | Helper function to log messages
logWith :: MonadIO m => LogAction IO Text -> Text -> m ()
logWith log msg = void . liftIO $ log <& msg

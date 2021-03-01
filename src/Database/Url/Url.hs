{-# LANGUAGE TemplateHaskell #-}

module Database.Url.Url
  ( Url(..)
  , UrlTable
  , TreeUrl
  , InsertUrl'(..)
  , QueryUrl'(..)
  , DeleteUrl'(..)
  , UpdateUUID'(..)
  , QueryUUID'(..)
  , insertUrl'
  , queryUrl'
  , deleteUrl'
  , updateUUID'
  , queryUUID'
  , urlIsNil
  ) where

import           Data.RedBlackTree
import           Data.Acid

import qualified Data.Text as T
import Data.SafeCopy
import Relude hiding (empty, find)
import Database.Common

-- | Url representation in database
data Url = Url
  { orig  :: OrigUrl  -- ^ original url
  , short :: ShortUrl -- ^ shortened version of url -- Unique
  } deriving (Show)

-- | Tree for url table
type TreeUrl  = RedBlackTree Url

-- | UrlTable of urls tree and id
-- id grows +1 with each genUUID
type UrlTable = Table (TreeUrl, UUID)

instance Ord Url where
  (<=) (Url s1 _) (Url s2 _) = s1 <= s2
  (<)  (Url s1 _) (Url s2 _) = s1 <  s2
  (>)  (Url s1 _) (Url s2 _) = s1 >  s2

instance Eq Url where
  (==) (Url _ s1) (Url _ s2) = s1 == s2

instance BinaryTreeNode Url where
  mergeNodes _ = id

instance SafeCopy Url where
  putCopy Url{..} = contain $ safePut orig
                           >> safePut short
  getCopy = contain $ Url <$> safeGet <*> safeGet

instance SafeCopy TreeUrl where
  putCopy = contain . safePut
  getCopy = contain   safeGet


-- | Inserts `Url` in db
insertUrl' :: Url -> Update UrlTable ()
insertUrl' = modify . (<$>) . first . flip insert

-- | Searches for `OrigUrl` by given `ShortUrl`
queryUrl' :: ShortUrl -> Query UrlTable (Maybe OrigUrl)
queryUrl' url = do
  Table (tree, _) <- ask
  return $ orig <$> find tree (Url T.empty url)

-- | Deletes `Url` by given `ShortUrl`
-- FIXME: write own tree traverse
deleteUrl' :: ShortUrl -> Update UrlTable ()
deleteUrl' url = undefined

-- | Adds +1 to `UUID`
updateUUID' :: Update UrlTable ()
updateUUID' = modify . (<$>) $ second (+1)

-- | Returns current value of `UUID`
queryUUID' :: Query UrlTable UUID
queryUUID' = do
  Table (_, uuid) <- ask
  return uuid


$(makeAcidic ''UrlTable  ['insertUrl', 'queryUrl', 'deleteUrl', 'updateUUID', 'queryUUID'])

--------- AUXILIARY FUNCTIONS ---------

-- | Checks if any of `Url` fields is nil
urlIsNil :: Url -> Bool
urlIsNil Url{..} = short == "" || orig == ""

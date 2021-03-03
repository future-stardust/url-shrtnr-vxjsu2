{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

import           Data.Acid
import           Data.SafeCopy
import           Relude

import           Database.Common
import qualified Database.Tree.Tree as BT

-- | Url representation in database
data Url = Url
  { orig  :: OrigUrl  -- ^ original url
  , short :: ShortUrl -- ^ shortened version of url -- Unique
  } deriving (Eq, Show)

-- | Tree for url table
type TreeUrl  = BT.Tree ShortUrl Url

-- | UrlTable of urls tree and id
type UrlTable = Table (TreeUrl, UUID)

instance BT.HasIndex Url ShortUrl where
  getIndex = short

instance SafeCopy Url where
  putCopy Url{..} = contain $ safePut orig
                           >> safePut short
  getCopy = contain $ Url <$> safeGet <*> safeGet

instance SafeCopy TreeUrl where
  putCopy (BT.toList -> list) = contain $ safePut list
  getCopy = contain $ BT.fromList <$> safeGet


-- | Inserts `Url` in db
insertUrl' :: Url -> Update UrlTable ()
insertUrl' = modify . (<$>) . first . BT.insert

-- | Searches for `OrigUrl` by given `ShortUrl`
queryUrl' :: ShortUrl -> Query UrlTable (Maybe OrigUrl)
queryUrl' url = do
  Table (tree, _) <- ask
  return $ orig <$> BT.lookup url tree

-- | Deletes `Url` by given `ShortUrl`
deleteUrl' :: ShortUrl -> Update UrlTable ()
deleteUrl' =  modify . (<$>) . first . BT.delete

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

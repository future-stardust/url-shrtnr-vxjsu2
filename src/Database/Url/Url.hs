{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
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
import qualified Database.Tree.Tree as T
-- import qualified Data.BTree.Pure as B

-- | Url representation in database
data Url = Url
  { orig  :: OrigUrl  -- ^ original url
  , short :: ShortUrl -- ^ shortened version of url -- Unique
  } deriving (Eq, Show)

-- | Tree for url table
type TreeUrl  = T.Tree ShortUrl Url

-- data Table b where
--   Table :: (T.Treeable a, T.Treeable b) => (a -> b) -> a -> UUID -> Table (b, UUID)

-- instance Functor Table where
--   fmap f (Table g a b) = Table (f . g) a b

-- | UrlTable of urls tree and id
type UrlTable = Table (TreeUrl, UUID)

instance T.HasIndex Url where
  type Index Url = ShortUrl
  getIndex = short

-- instance T.Treeable (B.Tree ShortUrl Url) where
--   type Elem (B.Tree ShortUrl Url) = Url
--   empty'          = B.empty B.twoThreeSetup
--   singleton' el   = B.singleton B.twoThreeSetup (T.getIndex el) el
--   insert' el tree = B.insert (T.getIndex el) el tree
--   lookup' k  tree = B.lookup k tree
--   delete' k  tree = B.delete k tree
--   toList'    tree = B.toList tree <&> snd
--   fromList'  el   = B.fromList B.twoThreeSetup $ (\x -> (T.getIndex x, x)) <$> el


instance SafeCopy Url where
  putCopy Url{..} = contain $ safePut orig
                           >> safePut short
  getCopy = contain $ Url <$> safeGet <*> safeGet

instance SafeCopy TreeUrl where
  putCopy (T.toList -> list) = contain $ safePut list
  getCopy = contain $ T.fromList <$> safeGet

-- | Inserts `Url` in db
insertUrl' :: Url -> Update UrlTable ()
insertUrl' = modify . (<$>) . first . T.insert

-- | Searches for `OrigUrl` by given `ShortUrl`
queryUrl' :: ShortUrl -> Query UrlTable (Maybe OrigUrl)
queryUrl' url = do
  Table (tree, _) <- ask
  return $ orig <$> T.lookup url tree

-- | Deletes `Url` by given `ShortUrl`
deleteUrl' :: ShortUrl -> Update UrlTable ()
deleteUrl' =  modify . (<$>) . first . T.delete

-- | Adds +1 to `UUID`
updateUUID' :: Update UrlTable ()
updateUUID' = modify . (<$>) $ second (+1)

-- | Returns current value of `UUID`
queryUUID' :: Query UrlTable UUID
queryUUID' = do
  Table (_, uuid) <- ask
  return uuid


$(makeAcidic ''UrlTable  ['insertUrl', 'queryUrl', 'deleteUrl', 'updateUUID', 'queryUUID'])


-- | Checks if any of `Url` fields is nil
urlIsNil :: Url -> Bool
urlIsNil Url{..} = short == "" || orig == ""

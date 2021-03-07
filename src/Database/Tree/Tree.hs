module Database.Tree.Tree
  ( HasIndex(..)
  , Tree(..)
  , Treeable(..)
  , empty
  , singleton
  , insert
  , lookup
  , delete
  , toList
  , fromList
  ) where

import Data.BTree.Primitives.Key (Key)
import qualified Data.BTree.Pure as BT
import Relude hiding (empty, toList, fromList)


-- | Specify over which field `r` should be indexed, `k` will be a key in `Tree`
class Key (Index r) => HasIndex r where
  type Index r
  getIndex :: r -> Index r

class HasIndex (Elem t) => Treeable t where
  type Elem t
  empty'     :: t
  singleton' :: Elem t -> t
  insert'    :: Elem t -> t -> t
  lookup'    :: Index (Elem t) -> t -> Maybe (Elem t)
  delete'    :: Index (Elem t) -> t -> t
  toList'    :: t -> [Elem t]
  fromList'  :: [Elem t] -> t

-- | Dependency injection of `BT.Tree`
-- TODO: make it with a separate typeclass
newtype Tree k v = Tree (BT.Tree k v)

-- | Default setup of B+-tree
treeSetup :: BT.TreeSetup
treeSetup = BT.twoThreeSetup

-- | Create empty tree
empty :: (HasIndex r) => Tree k r
empty = Tree $ BT.empty treeSetup

-- | Create tree with a single element
singleton :: (HasIndex r, k ~ Index r) => r -> Tree k r
singleton = Tree . (BT.singleton treeSetup =<< getIndex)

-- | Insert element into the `BT.Tree`
insert :: (HasIndex r, k ~ Index r) => r -> Tree k r -> Tree k r
insert el (Tree tree) = Tree $ BT.insert (getIndex el) el tree

-- | Lookup value by given key
lookup :: (HasIndex r, k ~ Index r) => k -> Tree k r -> Maybe r
lookup k (Tree tree) = BT.lookup k tree

-- | Delete value by given key
delete :: (HasIndex r, k ~ Index r) => k -> Tree k r -> Tree k r
delete k (Tree tree) = Tree $ BT.delete k tree

-- | Convert tree to list
toList :: (HasIndex r, k ~ Index r) => Tree k r -> [(k, r)]
toList (Tree tree) = BT.toList tree

-- | Convert list to tree
fromList :: (HasIndex r, k ~ Index r) => [(k, r)] -> Tree k r
fromList = Tree . BT.fromList treeSetup

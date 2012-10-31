
{-# LANGUAGE MultiParamTypeClasses #-}

module Map (
  Map(), split, empty, lookup, insert, delete, toList
  ) where

import Control.Arrow ((&&&))
import Monoid
import Prelude hiding (lookup,foldr)
import qualified Tree as T
import Tree (Tree, Measured(..))

data Elem k v = Elem { key :: k, value :: v }

-- Assuming the tree is maintained in order, then the Last element
-- in each subtree is also the greatest element in that subtree.

instance Ord k => Measured (Elem k v) (Last k) where
  measure (Elem k v) = Last k

type MapI k v = Tree (Last k) (Elem k v)

singleton :: k -> v -> MapI k v
singleton k v = T.singleton (Elem k v)

-- 3-way split: elements with keys <k, ==k, >k
split :: Ord k => k -> MapI k v -> (MapI k v, MapI k v, MapI k v)
split k t = case T.split (\i -> getLast i >= k) t of
  (l,r) -> case T.split (\i -> getLast i > k) r of
    (m,r) -> (l,m,r)

newtype Map k v = Map { unMap :: MapI k v }

empty :: Map k v
empty = Map T.empty

lookup :: Ord k => k -> Map k v -> Maybe v
lookup k (Map t) = case split k t of
  (_,m,_) -> fmap getFirst (foldMap (Just . First . value) m)

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v (Map t) = Map $ case split k t of
  (l,_,r) -> l <> singleton k v <> r

delete :: Ord k => k -> Map k v -> Map k v
delete k (Map t) = Map $ case split k t of
  (l,_,r) -> l <> r

toList :: Map k v -> [(k,v)]
toList = T.toList (key &&& value) . unMap


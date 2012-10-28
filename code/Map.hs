
{-# LANGUAGE MultiParamTypeClasses #-}

module Map (
  Map(), empty, lookup, insert, delete
  ) where

import Monoid
import Prelude hiding (lookup)
import qualified Tree as T
import Tree (Tree, Measured(..))

-- For internal use
data Elem k v = Elem { key :: k, value :: v }

instance Ord k => Measured (Elem k v) (Last k) where
  measure (Elem k v) = Last k

type MapI k v = Tree (Last k) (Elem k v)

singleton :: k -> v -> MapI k v
singleton k v = T.singleton (Elem k v)

split :: Ord k => k -> MapI k v -> (MapI k v, MapI k v)
split k = T.split (\i -> getLast i >= k)

split3 :: Ord k => k -> MapI k v -> (MapI k v, MapI k v, MapI k v)
split3 k t = case T.split (\i -> getLast i >= k) t of
  (l,r) -> case T.split (\i -> getLast i > k) r of
    (m,r) -> (l,m,r)

-- For export
newtype Map k v = Map { unMap :: MapI k v }

empty :: Map k v
empty = Map T.empty

lookup :: Ord k => k -> Map k v -> Maybe v
lookup k = fmap value . T.search p . unMap
  where
    p (Last i) = i >= k

insert :: (Ord k, Semigroup v) => k -> v -> Map k v -> Map k v
insert k v (Map t) = Map $ case split3 k t of
  (l,m,r) -> l <> singleton k (T.view1 v ((<> v) . foldMap1 value) m) <> r

delete :: Ord k => k -> Map k v -> Map k v
delete k (Map t) = Map $ case split3 k t of
  (l,m,r) -> l <> r


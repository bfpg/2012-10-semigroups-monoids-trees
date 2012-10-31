
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Tree (
  Tree(), Tree1(), Measured(..),
  empty, singleton, tree1, viewl, viewr,
  search, split, split1,
  toList, fromList
  ) where

import Monoid
import Prelude hiding (foldr)

class Semigroup v => Measured a v where
  measure :: a -> v

-- A non-empty binary tree, with values (type 'a') at leaves,
-- and measure annotations (type 'v') at internal nodes.
data Tree1 v a = Leaf a | Node v (Tree1 v a) (Tree1 v a)
  deriving (Show)

-- We don't care about shape, just left-to-right ordering.
instance Eq a => Eq (Tree1 v a) where
  t == u = toList t == toList u
    where
      toList = foldr (:) []

instance Foldable (Tree1 v) where
  foldMap = foldMap1

instance Foldable1 (Tree1 v) where
  foldMap1 f (Leaf x) = f x
  foldMap1 f (Node _ l r) = foldMap1 f l <> foldMap1 f r

instance Measured a v => Measured (Tree1 v a) v where
  measure (Leaf a) = measure a
  measure (Node v l r) = v

instance Measured a v => Semigroup (Tree1 v a) where
  -- Pretend we rebalance the tree here!
  l <> r = Node (measure l <> measure r) l r

-- Allow empty trees.
data Tree v a = Empty | Full (Tree1 v a)
  deriving (Eq,Show)

instance Foldable (Tree v) where
  foldMap f Empty = mempty
  foldMap f (Full t) = foldMap f t

instance Measured a v => Semigroup (Tree v a) where
  Empty <> r = r
  l <> Empty = l
  Full l <> Full r = Full (l <> r)

instance Measured a v => Monoid (Tree v a) where
  mempty = Empty

empty :: Tree v a
empty = Empty

singleton :: a -> Tree v a
singleton = Full . Leaf

-- Convert between Tree and Tree1.
tree1 :: b -> (Tree1 v a -> b) -> Tree v a -> b
tree1 e f Empty = e
tree1 e f (Full t) = f t

-- Extract an element from the left-hand end, continuation-passing style.
viewl :: Measured a v => b -> (a -> Tree v a -> b) -> Tree v a -> b
viewl e = tree1 e . viewl1
  where
    viewl1 f (Leaf x) = f x Empty
    viewl1 f (Node _ l r) = viewl1 (\x t -> f x (t <> Full r)) l

-- Extract an element from the right-hand end, continuation-passing style.
viewr :: Measured a v => b -> (Tree v a -> a -> b) -> Tree v a -> b
viewr e = tree1 e . viewr1
  where
    viewr1 f (Leaf x) = f Empty x
    viewr1 f (Node _ l r) = viewr1 (f . (Full l <>)) r

-- Find an element x in tree t, such that:
--   t = l <> singleton x <> r, for some l and r
--   p (measure l) == False
--   p (measure (l <> singleton x)) == True
-- If the element exists, return (Just x), otherwise Nothing.
search :: Measured a v => (v -> Bool) -> Tree v a -> Maybe a
search p (Full t) | p (measure t) = Just (left t)
  where
    left (Leaf a) = a
    left (Node _ l r)
      | p (measure l) = left l
      | otherwise = mid (measure l) r
    mid i (Leaf a) = a
    mid i (Node _ l r)
      | p (i <> measure l) = mid i l
      | otherwise = mid (i <> measure l) r
search _ _ = Nothing

-- Split a tree t into trees (l,r) such that all of:
--   t == l <> r
--   l is empty, or (p (measure l) == False)
--   r is empty, or there are x and s such that both:
--     r == singleton x <> s
--     p (measure (l <> singleton x)) == True
split :: Measured a v => (v -> Bool) -> Tree v a -> (Tree v a, Tree v a)
split _ Empty = (Empty, Empty)
split p (Full t) = split1 p t

split1 :: Measured a v => (v -> Bool) -> Tree1 v a -> (Tree v a, Tree v a)
split1 p t
  | p (measure t) = left t
  | otherwise = (Full t, Empty)
  where
    -- left: used when looking along the left edge of the tree, so there is no
    --   prefix to seed the accumulated measurement.
    -- Leaf: measure predicate already known true, so value goes to the right.
    left (Leaf a) = (Empty, singleton a)
    -- Node: predicate on node known true, but is transition in left subtree?
    left (Node _ l r)
      -- predicate on left subtree is true, so look for transition on left.
      | p m = case left l of (ll,lr) -> (ll, lr <> Full r)
      -- predicate on left subtree is false, so look for transition on right.
      | otherwise = case mid m r of (rl,rr) -> (Full l <> rl, rr)
      where m = measure l
    -- mid: used when looking away from the left edge of tree, so we need an
    --   extra parameter to seed the accumulated measure.
    mid i (Leaf a) = (Empty, singleton a)
    mid i (Node _ l r)
      | p m = case mid i l of (ll,lr) -> (ll, lr <> Full r)
      | otherwise = case mid m r of (rl,rr) -> (Full l <> rl, rr)
      where m = i <> measure l

toList :: (a -> b) -> Tree v a -> [b]
toList f = foldr ((:).f) []

-- For testing, build a mostly-balanced tree.
fromList :: Measured b v => (a -> b) -> [a] -> Tree v b
fromList f = merges . map (singleton.f)
  where
    merges [] = empty
    merges [t] = t
    merges ts = merges (pairs ts)
    pairs (t:u:vs) = (t <> u) : pairs vs
    pairs vs = vs


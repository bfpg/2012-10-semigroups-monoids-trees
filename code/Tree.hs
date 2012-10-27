
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Tree where

import Monoid

class Semigroup v => Measured a v where
  measure :: a -> v

data Tree1 v a = Leaf a | Node v (Tree1 v a) (Tree1 v a)
  deriving (Show)

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

data Tree v a = Empty | Full (Tree1 v a)
  deriving (Show)

instance Foldable (Tree v) where
  foldMap f Empty = mempty
  foldMap f (Full t) = foldMap f t

instance Measured a v => Semigroup (Tree v a) where
  Empty <> r = r
  l <> Empty = l
  Full l <> Full r = Full (l <> r)

instance Measured a v => Monoid (Tree v a) where
  mempty = Empty

singleton :: a -> Tree v a
singleton = Full . Leaf

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

split :: Measured a v => (v -> Bool) -> Tree v a -> (Tree v a, Tree v a)
split _ Empty = (Empty, Empty)
split p (Full t)
  | p (measure t) = left t
  | otherwise = (Full t, Empty)
  where
    left (Leaf a) = (Empty, singleton a)
    left (Node _ l r)
      | p m = case left l of (ll,lr) -> (ll, lr <> Full r)
      | otherwise = case mid m r of (rl,rr) -> (Full l <> rl, rr)
      where m = measure l
    mid i (Leaf a) = (Empty, singleton a)
    mid i (Node _ l r)
      | p m = case mid i l of (ll,lr) -> (ll, lr <> Full r)
      | otherwise = case mid m r of (rl,rr) -> (Full l <> rl, rr)
      where m = i <> measure l


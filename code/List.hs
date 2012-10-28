
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module List (
  List(), empty, singleton, viewl, viewr, split, insert, delete, (!!)
  ) where

import Monoid
import Prelude hiding (elem,(!!))
import qualified Tree as T
import Tree (Tree, Measured)

newtype Elem a = Elem { elem :: a }
newtype List a = List { list :: Tree (Sum Int) (Elem a) }

instance Measured (Elem a) (Sum Int) where
  measure _ = Sum 1

instance Semigroup (List a) where
  List l <> List r = List (l <> r)

instance Monoid (List a) where
  mempty = empty

instance Foldable List where
  foldMap f = foldMap (f . elem) . list

empty :: List a
empty = List T.empty

singleton :: a -> List a
singleton = List . T.singleton . Elem

viewl :: b -> (a -> List a -> b) -> List a -> b
viewl e f = T.viewl e (\x t -> f (elem x) (List t)) . list

viewr :: b -> (List a -> a -> b) -> List a -> b
viewr e f = T.viewr e (\t x -> f (List t) (elem x)) . list

split :: Int -> List a -> (List a, List a)
split n (List t) = case T.split p t of (l,r) -> (List l, List r)
  where
    p (Sum i) = i < n

insert :: Int -> a -> List a -> List a
insert n x xs = case split n xs of (l,r) -> l <> singleton x <> r

delete :: Int -> List a -> List a
delete n xs = case split n xs of (l,r) -> viewl xs (\_ -> (l <>)) r

(!!) :: List a -> Int -> Maybe a
(List t) !! n = fmap elem (T.search p t)
  where
    p (Sum i) = i < n


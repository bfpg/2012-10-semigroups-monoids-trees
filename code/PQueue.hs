
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module PQueue (
  PQueue(), empty, singleton, push, pop
  ) where

import Monoid
import qualified Tree as T
import Tree (Tree, Measured(..))

class (Ord p, Semigroup (m p)) => Ordered m p where
  (.<=.) :: m p -> m p -> Bool
  wrap :: p -> m p
  unwrap :: m p -> p

instance Ord p => Ordered Min p where
  Min x .<=. Min y = x <= y
  wrap = Min
  unwrap = getMin

instance Ord p => Ordered Max p where
  Max x .<=. Max y = y <= x
  wrap = Max
  unwrap = getMax

data Elem m p a = Elem { priority :: m p, value :: a }
newtype PQueue m p a = PQueue { pqueue :: Tree (m p) (Elem m p a) }

type MinPQ p a = PQueue Min p a
type MaxPQ p a = PQueue Max p a

instance Ordered m p => Measured (Elem m p a) (m p) where
  measure (Elem p x) = p

instance Ordered m p => Semigroup (PQueue m p a) where
  PQueue l <> PQueue r = PQueue (l <> r)

instance Ordered m p => Monoid (PQueue m p a) where
  mempty = empty

empty :: PQueue m p a
empty = PQueue T.empty

singleton :: Ordered m p => p -> a -> PQueue m p a
singleton p x = PQueue (T.singleton (Elem (wrap p) x))

push :: Ordered m p => p -> a -> PQueue m p a -> PQueue m p a
push p x q = q <> singleton p x

pop :: Ordered m p => b -> (a -> PQueue m p a -> b) -> PQueue m p a -> b
pop e f (PQueue t) = T.view1 e g t
  where
    g t = case T.split1 (.<=. measure t) t of
      (l,r) -> T.viewl e (\x r -> f (value x) (PQueue (l <> r))) r


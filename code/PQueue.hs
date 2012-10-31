
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module PQueue (
  PQueue(), Ordered(..), empty, singleton, push, pop,
  toList, fromList
  ) where

import Monoid
import Prelude hiding (foldr,elem)
import qualified Tree as T
import Tree (Tree, Measured(..))

-- Generalise over min-priority and max-priority queues.

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

-- New elements are simply appended, so the tree is maintained in
-- order of arival. For efficiency, we would need the tree to maintain
-- balance.
push :: Ordered m p => p -> a -> PQueue m p a -> PQueue m p a
push p x q = q <> singleton p x

elem :: Ordered m p => Elem m p a -> (p,a)
elem (Elem p v) = (unwrap p, v)

-- Search for the minimum (maximum) element using the Min (Max) semigroup
-- values cached in tree nodes. The measure at the tree root tells us the
-- minimum (maximum) priority we are looking for.
pop :: Ordered m p => b -> (p -> a -> PQueue m p a -> b) -> PQueue m p a -> b
pop e f (PQueue t) = T.tree1 e g t
  where
    g t = case T.split1 (.<=. measure t) t of
      (l,r) -> T.viewl e (\x r -> uncurry f (elem x) (PQueue (l <> r))) r

toList :: Ordered m p => PQueue m p a -> [(p,a)]
toList = T.toList elem . pqueue

fromList :: Ordered m p => [(p,a)] -> PQueue m p a
fromList = PQueue . T.fromList (\(p,x) -> Elem (wrap p) x)


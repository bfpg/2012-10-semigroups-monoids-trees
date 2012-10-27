
module Monoid where

import Prelude hiding(foldl,foldr)
import Data.Maybe

infixr 6 <>

class Semigroup s where
  (<>) :: s -> s -> s

  sconcatMap :: Foldable1 t => (a -> s) -> t a -> s
  sconcatMap = foldMap1

  sconcat :: Foldable1 t => t s -> s
  sconcat = sconcatMap id

class Semigroup m => Monoid m where
  mempty :: m

  mconcatMap :: Foldable t => (a -> m) -> t a -> m
  mconcatMap = foldMap

  mconcat :: Foldable t => t m -> m
  mconcat = mconcatMap id

class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m

  fold :: Monoid m => t m -> m
  fold = foldMap id

  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f z t = appEndo (foldMap (Endo . f) t) z

  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z

class Foldable t => Foldable1 t where
  foldMap1 :: Semigroup s => (a -> s) -> t a -> s

  fold1 :: Semigroup s => t s -> s
  fold1 = foldMap1 id

  foldr1 :: (a -> a -> a) -> t a -> a
  foldr1 f = fromMaybe (error "foldr1: empty structure")
    . foldr g Nothing
    where g = flip (maybe Just ((Just.).f))

  foldl1 :: (a -> a -> a) -> t a -> a
  foldl1 f = fromMaybe (error "foldl1: empty structure")
    . foldl g Nothing
    where g = maybe Just ((Just.).f)

instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []

instance Semigroup b => Semigroup (a -> b) where
  (f <> g) x = f x <> g x

instance Monoid b => Monoid (a -> b) where
  mempty _ = mempty

instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
  (a1,b1) <> (a2,b2) = (a1<>a2, b1<>b2)

instance (Monoid a, Monoid b) => Monoid (a,b) where
  mempty = (mempty, mempty)

newtype Dual a = Dual { getDual :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup a => Semigroup (Dual a) where
  Dual x <> Dual y = Dual (y <> x)

instance Monoid a => Monoid (Dual a) where
  mempty = Dual mempty

newtype Endo a = Endo { appEndo :: a -> a }

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id

newtype All = All { getAll :: Bool }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup All where
  All x <> All y = All (x && y)

instance Monoid All where
  mempty = All True

all :: Foldable t => (a -> Bool) -> t a -> Bool
all p = getAll . foldMap (All . p)

newtype Any = Any { getAny :: Bool }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup Any where
  Any x <> Any y = Any (x || y)

instance Monoid Any where
  mempty = Any False

any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = getAny . foldMap (Any . p)

newtype Sum a = Sum { getSum :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x + y)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

newtype Product a = Product { getProduct :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Semigroup (Product a) where
  Product x <> Product y = Product (x * y)

instance Num a => Monoid (Product a) where
  mempty = Product 1

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

instance Semigroup s => Semigroup (Maybe s) where
  Just x <> Just y = Just (x <> y)
  Nothing <> y = y
  x <> Nothing = x

instance Semigroup s => Monoid (Maybe s) where
  mempty = Nothing

newtype First a = First { getFirst :: a }
  deriving (Eq, Ord, Read, Show)

instance Semigroup (First a) where
  First x <> First y = First x

head1 :: Foldable1 t => t a -> a
head1 = getFirst . foldMap1 First

head :: Foldable t => t a -> Maybe a
head = fmap getFirst . foldMap (Just . First)

findFirst :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
findFirst f = fmap getFirst . foldMap (fmap First . f)

newtype Last a = Last { getLast :: a }
  deriving (Eq, Ord, Read, Show)

instance Semigroup (Last a) where
  Last x <> Last y = Last y

last1 :: Foldable1 t => t a -> a
last1 = getLast . foldMap1 Last

last :: Foldable t => t a -> Maybe a
last = fmap getLast . foldMap (Just . Last)

findLast :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
findLast f = fmap getLast . foldMap (fmap Last . f)

newtype Min a = Min { getMin :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Ord a => Semigroup (Min a) where
  Min x <> Min y = Min (min x y)

minimum1 :: (Ord a, Foldable1 t) => t a -> a
minimum1 = getMin . foldMap1 Min

minimum :: (Ord a, Foldable t) => t a -> Maybe a
minimum = fmap getMin . foldMap (Just . Min)

newtype Max a = Max { getMax :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Ord a => Semigroup (Max a) where
  Max x <> Max y = Max (max x y)

maximum1 :: (Ord a, Foldable1 t) => t a -> a
maximum1 = getMax . foldMap1 Max

maximum :: (Ord a, Foldable t) => t a -> Maybe a
maximum = fmap getMax . foldMap (Just . Max)

instance Foldable Maybe where
  foldMap f Nothing = mempty
  foldMap f (Just x) = f x

instance Foldable [] where
  foldMap f [] = mempty
  foldMap f (x:xs) = f x <> foldMap f xs


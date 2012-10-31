
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, KindSignatures #-}

import qualified Monoid as M
import qualified PQueue as Q

import Control.Arrow ((&&&))
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import Data.Sequence ((|>))

import Test.QuickCheck -- V2

-- Test PQueue by simulating a sequence of operations, and comparing the
-- execution trace with a simple list-based alternative implementation.

type Op p a = Maybe (p,a)

data Trace p a = TrEmpty | TrPop p a [(p,a)] | TrPush [(p,a)]
  deriving (Eq)

class (Eq a, Q.Ordered m p) => Queue q m p a where
  pop :: b -> (p -> a -> q m p a -> b) -> q m p a -> b
  push :: p -> a -> q m p a -> q m p a
  toList :: q m p a -> [(p,a)]
  empty :: q m p a

instance (Eq a, Q.Ordered m p) => Queue Q.PQueue m p a where
  toList = Q.toList
  empty = Q.empty
  push = Q.push
  pop = Q.pop

newtype Sim (m :: * -> *) p a = Sim { sim :: S.Seq (p,a) }

toSim :: Queue q m p a => q m p a -> Sim m p a
toSim q = Sim (S.fromList (toList q))

instance (Eq a, Q.Ordered m p) => Queue Sim m p a where
  toList = F.toList . sim
  empty = Sim S.empty
  push p x (Sim q) = Sim (q |> (p,x))
  pop e f (Sim q) = maybe e extract (F.foldr fmin Nothing q)
    where
      fmin (p,x) Nothing = Just p
      fmin (p,x) (Just p1)
        | (Q.wrap p :: m p) Q..<=. Q.wrap p1 = Just p
        | otherwise = Just p1
      extract p = f p x (Sim (l S.>< rr))
        where
          (l,r) = S.breakl (\(p1,x) -> p == p1) q
          (_,x) S.:< rr = S.viewl r

op :: Queue q m p a => Op p a -> q m p a -> (q m p a, Trace p a)
op Nothing = pop (empty, TrEmpty) (\p x q -> (q, TrPop p x (toList q)))
op (Just (p,x)) = (id &&& TrPush . toList) . push p x

run :: Queue q m p a => q m p a -> [Op p a] -> [Trace p a]
run q (p:ps) = case op p q of (qr,trace) -> trace : run qr ps
run q [] = []

prop_sim :: Queue q m p a => q m p a -> [Op p a] -> Bool
prop_sim q ops = run q ops == run (toSim q) ops

prop_nontrivial :: Queue q m p a => q m p a -> [Op p a] -> [Op p a] -> Property
prop_nontrivial q ops1 ops2 = ops1 /= ops2 ==>
  run q ops1 /= run (toSim q) ops2

-- Test from another angle: generate a trace, then check that it is valid
-- according to the semantics of the operations.
prop_trace :: forall q m p a. Queue q m p a => q m p a -> [Op p a] -> Bool
prop_trace q ops = check (toList q) ops (run q ops)
  where
    check _ [] [] = True
    check [] (Nothing : ops) (TrEmpty : tr) = check [] ops tr
    check q0 (Nothing : ops) (TrPop p x q1 : tr) = check_pop (p,x) q0 q1 && check q1 ops tr
    check q0 (Just e : ops) (TrPush q1 : tr) = q1 == q0 ++ [e] && check q1 ops tr
    check _ _ _ = False
    check_pop e (e0:q0) q1 | e == e0 = q0 == q1
    check_pop e (e0:q0) (e1:q1) = fst e .<. fst e0 && e0 == e1 && check_pop e q0 q1
    check_pop _ _ _ = False
    (.<.) :: p -> p -> Bool
    p0 .<. p1 = p0 /= p1 && (Q.wrap p0 :: m p) Q..<=. Q.wrap p1

main = do
  quickCheck (prop_trace (empty :: Q.PQueue M.Max Integer Integer))
  quickCheck (prop_nontrivial (empty :: Q.PQueue M.Max Integer Integer))
  quickCheck (prop_sim (empty :: Q.PQueue M.Max Integer Integer))
  quickCheck (prop_trace (empty :: Q.PQueue M.Min Integer Integer))
  quickCheck (prop_nontrivial (empty :: Q.PQueue M.Min Integer Integer))
  quickCheck (prop_sim (empty :: Q.PQueue M.Min Integer Integer))


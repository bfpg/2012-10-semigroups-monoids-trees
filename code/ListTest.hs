
import Monoid as M
import List as L

import Control.Arrow ((***))
import Test.QuickCheck -- V2

prop_append :: [Int] -> [Int] -> Bool
prop_append xs ys = toList (fromList xs <> fromList ys) == xs ++ ys

prop_viewl :: Int -> [Int] -> Bool
prop_viewl x xs = viewl False (\y r -> y == x) (fromList (x:xs))

prop_viewl_nontrivial :: [Int] -> Bool
prop_viewl_nontrivial l = not (viewl False (\_ _ -> False) (fromList l))

prop_viewr :: Int -> [Int] -> Bool
prop_viewr x xs = viewr False (\r y -> y == x) (fromList (xs ++ [x]))

prop_viewr_nontrivial :: [Int] -> Bool
prop_viewr_nontrivial l = not (viewr False (\_ _ -> False) (fromList l))

prop_length :: [Int] -> Bool
prop_length xs = L.length (fromList xs) == Prelude.length xs

prop_split_lt :: NonNegative Int -> [Int] -> Property
prop_split_lt (NonNegative n) xs = (n < Prelude.length xs) ==>
  (toList *** toList) (split n (fromList xs)) == splitAt n xs

prop_split_ge :: Int -> [Int] -> Property
prop_split_ge n xs = (n >= Prelude.length xs) ==>
  (toList *** toList) (split n (fromList xs)) == (xs,[])

prop_insert_lt :: NonNegative Int -> Int -> [Int] -> Property
prop_insert_lt (NonNegative n) x xs = (n < Prelude.length xs) ==>
  toList (insert n x (fromList xs)) == (\(l,r) -> l ++ x:r) (splitAt n xs)

prop_insert_ge :: Int -> Int -> [Int] -> Property
prop_insert_ge n x xs = (n >= Prelude.length xs) ==>
  toList (insert n x (fromList xs)) == xs ++ [x]

prop_delete_lt :: NonNegative Int -> [Int] -> Property
prop_delete_lt (NonNegative n) xs = (n < Prelude.length xs) ==>
  toList (delete n (fromList xs)) == (\(l,r) -> l ++ tail r) (splitAt n xs)

prop_delete_ge :: Int -> [Int] -> Property
prop_delete_ge n xs = (n >= Prelude.length xs) ==>
  toList (delete n (fromList xs)) == xs

prop_bang_lt :: NonNegative Int -> [Int] -> Property
prop_bang_lt (NonNegative n) xs = (n < Prelude.length xs) ==>
  fromList xs L.!! n == Just (xs Prelude.!! n)

prop_bang_ge :: Int -> [Int] -> Property
prop_bang_ge n xs = (n >= Prelude.length xs) ==>
  fromList xs L.!! n == Nothing

main = do
  quickCheck prop_append
  quickCheck prop_viewl
  quickCheck prop_viewl_nontrivial
  quickCheck prop_viewr
  quickCheck prop_viewr_nontrivial
  quickCheck prop_length
  quickCheck prop_split_lt
  quickCheck prop_split_ge
  quickCheck prop_insert_lt
  quickCheck prop_insert_ge
  quickCheck prop_delete_lt
  quickCheck prop_delete_ge
  quickCheck prop_bang_lt
  quickCheck prop_bang_ge


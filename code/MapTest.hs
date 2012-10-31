
import qualified Monoid as Mo

import qualified Map as M1
import qualified Data.Map as M2

import Test.QuickCheck -- V2

-- Integer sizes better than Int for this test.
type I = Integer
type MGen = [(I, Maybe I)]

mkM1 :: MGen -> M1.Map I I
mkM1 = foldr f M1.empty where
  f (k,v) = maybe (M1.delete k) (M1.insert k) v

mkM2 :: MGen -> M2.Map I I
mkM2 = foldr f M2.empty where
  f (k,v) = maybe (M2.delete k) (M2.insert k) v

prop_lookup :: MGen -> Bool
prop_lookup kv = all (\(k,_) -> M1.lookup k m1 == M2.lookup k m2) kv
  where
    m1 = mkM1 kv
    m2 = mkM2 kv

prop_toList :: MGen -> Bool
prop_toList kv = M1.toList (mkM1 kv) == M2.toList (mkM2 kv)

main = do
  quickCheck prop_lookup
  quickCheck prop_toList


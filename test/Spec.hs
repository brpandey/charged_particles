module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Vec (Vec2 (..), VectorSpace (..))

-- Arbitrary instance so QuickCheck can generate Vec2 values
instance Arbitrary Vec2 where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Vec2 (x, y))

-- === Properties with counterexamples ===

prop_add_comm :: Vec2 -> Vec2 -> Property
prop_add_comm v1 v2 =
  counterexample
    ("v1 = " ++ show v1 ++ ", v2 = " ++ show v2)
    (vadd v1 v2 == vadd v2 v1)

prop_scale_distrib :: Float -> Vec2 -> Vec2 -> Property
prop_scale_distrib s v1 v2 =
  counterexample
    ("s = " ++ show s ++ ", v1 = " ++ show v1 ++ ", v2 = " ++ show v2)
    (vscale s (vadd v1 v2) == vadd (vscale s v1) (vscale s v2))

prop_norm_unit :: Vec2 -> Property
prop_norm_unit v =
  counterexample
    ("v = " ++ show v ++ ", vnorm v = " ++ show (vnorm v))
    (v == Vec2 (0, 0) || abs (vmag (vnorm v) - 1) < 1e-6)

-- === Main ===
main :: IO ()
main = hspec $ do
  describe "VectorSpace Vec2 laws" $ do
    it "addition is commutative" $
      property prop_add_comm

    it "scaling distributes over addition" $
      property prop_scale_distrib

    it "norm produces unit vector (except zero)" $
      property prop_norm_unit

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


module Linear.V3Spec (
  spec
) where

import           Test.Hspec
import           Test.QuickCheck

import           Linear.Epsilon
import           Linear.Metric
import           Linear.Vector

import           Linear.V3.Arbitrary

-- | test for the property `|v| != 0`
prop_NonZeroV3_isNonZero :: (Epsilon a, Floating a) => NonZeroV3 a -> Bool
prop_NonZeroV3_isNonZero (NonZeroV3 v) = not . nearZero $ norm v

-- | test for the property `|v| = 1`
prop_UnitV3_isUnit :: (Epsilon a, Floating a) => UnitV3 a -> Bool
prop_UnitV3_isUnit (UnitV3 v) = nearZero $ norm v - 1

-- | test for the property `|v| = 1`
prop_CartesianUnitV3_isUnit :: (Epsilon a, Floating a) => CartesianUnitV3 a -> Bool
prop_CartesianUnitV3_isUnit (CartesianUnitV3 v) = nearZero $ norm v - 1

-- | test for the property `|u·v| = 1 or 0`
prop_CartesianUnitV3_isOrthogonal :: (Epsilon a, Floating a) => CartesianUnitV3 a -> CartesianUnitV3 a -> Bool
prop_CartesianUnitV3_isOrthogonal (CartesianUnitV3 u) (CartesianUnitV3 v) = nearZero uv || nearZero (uv - 1) where
  uv = abs $ u `dot` v

-- | test for the property that all vectors are orthogonal and normal
prop_BasisV3_isOrthonormal  :: (Epsilon a, Floating a) => BasisV3 a -> Bool
prop_BasisV3_isOrthonormal (BasisV3 (x,y,z)) =
  nearZero (dot x y) && nearZero (dot y z) && nearZero (dot z x)
    && nearZero (norm x - 1) && nearZero (norm y - 1) && nearZero (norm z - 1)

-- | test for the property `|v*c| = c` for unit vector v and arbitrary scalar c
prop_Metric_V3_norm :: (Epsilon a, Floating a) => UnitV3 a -> a -> Bool
prop_Metric_V3_norm (UnitV3 v) c = nearZero $ norm (v ^* c) - (abs c)

-- | test for the property `signorm(v*c) = v` for unit vector v and c >= 0
prop_Metric_V3_signorm :: (Epsilon a, Floating a) => UnitV3 a -> NonZero a -> Bool
prop_Metric_V3_signorm (UnitV3 v) c = nearZero $ signorm (v ^* (abs (getNonZero c))) - v



spec :: Spec
spec = specTyped @Double

specTyped :: forall a. (Eq a, Show a, Arbitrary a, Epsilon a, Floating a) => Spec
specTyped = do
  describe "V3" $ do
    describe "Arbitrary" $ do
      describe "NonZeroV3" $ do
        it "satisfies the property `|v| != 0`" $ do
          property $ prop_NonZeroV3_isNonZero @a
      describe "UnitV3" $ do
        it "satisfies the property `|v| = 1`" $ do
          property $ prop_UnitV3_isUnit @a
      describe "CartesianUnitV3" $ do
        it "satisfies the property `|v| = 1`" $ do
          property $ prop_CartesianUnitV3_isUnit @a
        it "satisfies the property `|u·v| = 1 or 0`" $ do
          property $ prop_CartesianUnitV3_isOrthogonal @a
      describe "BasisV3" $ do
        it "satisfies the property that vectors are orthonormal" $ do
          property $ prop_BasisV3_isOrthonormal @a
    describe "Metric" $ do
      it "satisfies property `|v*c| = c` for unit vectors" $
        property $ prop_Metric_V3_norm @a
      it "satisfies property `signorm(v*c) = v` for unit vectors" $
        property $ prop_Metric_V3_signorm @a

module Linear.V3Spec (
  spec
) where

import           Test.Hspec
import           Test.QuickCheck

import           Linear.Epsilon
import           Linear.Metric

import           Linear.V3.Arbitrary


-- | test for the property `|v| = 1`
prop_UnitV3_isUnit :: UnitV3 Double -> Bool
prop_UnitV3_isUnit (UnitV3 v) = nearZero $ norm v - 1

spec :: Spec
spec = do
  describe "V3" $ do
    describe "Arbitrary" $ do
      describe "UnitV3" $ do
        it "satisfies the property `|v| = 1`" $ do
          property prop_UnitV3_isUnit

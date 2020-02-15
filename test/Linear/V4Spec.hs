module Linear.V4Spec (
  spec
) where

import           Test.Hspec
import           Test.QuickCheck

import           Linear.Epsilon
import           Linear.Metric

import           Linear.V4.Arbitrary


-- | test for the property `|v| = 1`
prop_UnitV4_isUnit :: UnitV4 Double -> Bool
prop_UnitV4_isUnit (UnitV4 v) = nearZero $ norm v - 1

spec :: Spec
spec = do
  describe "V4" $ do
    describe "Arbitrary" $ do
      describe "UnitV4" $ do
        it "satisfies the property `|v| = 1`" $ do
          property prop_UnitV4_isUnit

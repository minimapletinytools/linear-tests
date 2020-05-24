{-|
Module      : Linear.V4Spec
Description : property tests for 'Linear.V4' and 'Linear.V4.Arbitrary'
Maintainer  : chippermonky@gmail.com
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


module Linear.V4Spec (
  spec
) where

import           Test.Hspec
import           Test.QuickCheck

import           Linear.Epsilon
import           Linear.Metric

import           Linear.V4.Arbitrary


-- | test for the property `|v| != 0`
prop_NonZeroV4_isNonZero :: (Epsilon a, Floating a) => NonZeroV4 a -> Bool
prop_NonZeroV4_isNonZero (NonZeroV4 v) = not . nearZero $ norm v

-- | test for the property `|v| = 1`
prop_UnitV4_isUnit :: (Epsilon a, Floating a) => UnitV4 a -> Bool
prop_UnitV4_isUnit (UnitV4 v) = nearZero $ norm v - 1

spec :: Spec
spec = specTyped @Double

specTyped :: forall a. (Eq a, Show a, Arbitrary a, Epsilon a, Floating a) => Spec
specTyped = do
  describe "V4" $ do
    describe "Arbitrary" $ do
      describe "NonZeroV4" $ do
        it "satisfies the property `|v| != 0`" $ do
          property $ prop_NonZeroV4_isNonZero @a
      describe "UnitV4" $ do
        it "satisfies the property `|v| = 1`" $ do
          property $ prop_UnitV4_isUnit @a

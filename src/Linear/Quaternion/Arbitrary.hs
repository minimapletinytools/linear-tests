{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linear.Quaternion.Arbitrary (
) where

import           Linear.Epsilon
import           Linear.Matrix
import           Linear.Quaternion
--import           Linear.Metric
import           Linear.V3.Arbitrary

import           Test.QuickCheck

instance (Arbitrary a, Epsilon a, Floating a) => Arbitrary (Quaternion a) where
  arbitrary = do
    UnitV3 v <- arbitrary
    r <- arbitrary
    return $ axisAngle v r

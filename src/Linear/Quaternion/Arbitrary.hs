{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linear.Quaternion.Arbitrary (
  CartesianQuaternion(..)
) where

import           Linear.Epsilon
import           Linear.Matrix       ()
import           Linear.Quaternion
import           Linear.V3.Arbitrary

import           Test.QuickCheck

-- | `Arbitrary Quaternion` instance
instance (Arbitrary a, Epsilon a, Floating a) => Arbitrary (Quaternion a) where
  arbitrary = do
    UnitV3 v <- arbitrary
    r <- arbitrary
    return $ axisAngle v r

-- | Arbitrary instances of this type are restricted to increment of 90 degrees along cartesian axis
newtype CartesianQuaternion a = CartesianQuaternion { unCartesianQuaternion :: Quaternion a } deriving (Show)

instance (Arbitrary a, Epsilon a, Floating a) => Arbitrary (CartesianQuaternion a) where
  arbitrary = do
    CartesianUnitV3 v <- arbitrary
    r <- elements [0, pi/2, pi, 3*pi/2]
    return . CartesianQuaternion $ axisAngle v r

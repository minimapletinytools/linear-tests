{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linear.V3.Arbitrary (
  UnitV3(..)
  , NonZeroV3(..)
  , CartesianUnitV3(..)
  , BasisV3(..)
) where

import           Linear.Epsilon
import           Linear.Metric
import           Linear.V3
import           Linear.Vector

import           Test.QuickCheck

-- | `Arbitrary V3` has no restrictions on components
instance (Arbitrary a) => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

-- | `Arbitrary NonZero` is never the zero vector
newtype NonZeroV3 a = NonZeroV3 {unNonZeroV3 :: V3 a} deriving (Show)

instance (Arbitrary a, Epsilon a, Floating a) => Arbitrary (NonZeroV3 a) where
  arbitrary = do
    v <- arbitrary `suchThat` (not . nearZero)
    return $ NonZeroV3 v

-- | `Arbitrary UnitV3` always has norm 1
newtype UnitV3 a = UnitV3 {unUnitV3 :: V3 a}  deriving (Show)

instance (Arbitrary a, Epsilon a, Floating a) => Arbitrary (UnitV3 a) where
  arbitrary = do
    v <- V3 <$> arbitrary <*> arbitrary <*> (arbitrary `suchThat` (not . nearZero))
    return . UnitV3 . signorm $ v

-- | `Arbitrary CartesianUnitV3` is a unit vector along cartesian axis
newtype CartesianUnitV3 a = CartesianUnitV3 {unCartesianUnitV3 :: V3 a}  deriving (Show)

instance (Arbitrary a, Epsilon a, Floating a) => Arbitrary (CartesianUnitV3 a) where
  arbitrary = elements $ CartesianUnitV3 <$> [unit _x, unit _y, unit _z, - (unit _x), - (unit _y), - (unit _z)]

-- | `Aribtrary BasisV3` is a orthonormal set of vectors
newtype BasisV3 a = BasisV3 { unBasisV3 :: (V3 a, V3 a, V3 a) } deriving (Show)

instance (Arbitrary a, Epsilon a, Floating a) => Arbitrary (BasisV3 a) where
  arbitrary = do
    UnitV3 x <- arbitrary
    o <- arbitrary `suchThat` (not . nearZero . dot x)
    let
      y = signorm $ cross x o
      z = cross x y
    return $ BasisV3 (x, y, z)

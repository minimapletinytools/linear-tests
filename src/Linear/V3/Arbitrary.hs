{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linear.V3.Arbitrary (
  UnitV3(..)
) where

import           Linear.Epsilon
import           Linear.Metric
import           Linear.V3

import           Test.QuickCheck

-- | no restrictions on components by default
instance (Arbitrary a) => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

newtype UnitV3 a = UnitV3 {unUnitV3 :: V3 a}  deriving (Show)

instance (Arbitrary a, Epsilon a, Floating a) => Arbitrary (UnitV3 a) where
  arbitrary = do
    v <- V3 <$> arbitrary <*> arbitrary <*> (arbitrary `suchThat` (not . nearZero))
    return . UnitV3 . signorm $ v

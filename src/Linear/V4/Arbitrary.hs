{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linear.V4.Arbitrary (
  UnitV4(..)
) where

import           Linear.Epsilon
import           Linear.Metric
import           Linear.V4

import           Test.QuickCheck

-- | `Arbitrary V4` instances have no restrictions on components
instance (Arbitrary a) => Arbitrary (V4 a) where
  arbitrary = V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | `Arbitrary UnitV4` instances always have norm 1
newtype UnitV4 a = UnitV4 {unUnitV4 :: V4 a}  deriving (Show)

instance (Arbitrary a, Epsilon a, Floating a) => Arbitrary (UnitV4 a) where
  arbitrary = do
    v <- V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> (arbitrary `suchThat` (not . nearZero))
    return . UnitV4 . signorm $ v

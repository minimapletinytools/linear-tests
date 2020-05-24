{-|
Module      : Linear.V4.Arbitrary
Description : 'Arbitrary' instances for 'Linear.V4'
Maintainer  : chippermonky@gmail.com
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linear.V4.Arbitrary (
  UnitV4(..)
  , NonZeroV4(..)
) where

import           Linear.Epsilon
import           Linear.Metric
import           Linear.V4

import           Test.QuickCheck

-- | `Arbitrary V4` has no restrictions on components
instance (Arbitrary a) => Arbitrary (V4 a) where
  arbitrary = V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | `Arbitrary NonZero` is never the zero vector
newtype NonZeroV4 a = NonZeroV4 {unNonZeroV4 :: V4 a} deriving (Show)

instance (Arbitrary a, Epsilon a, Floating a) => Arbitrary (NonZeroV4 a) where
  arbitrary = do
    v <- arbitrary `suchThat` (not . nearZero)
    return $ NonZeroV4 v

-- | `Arbitrary UnitV4` always has norm 1
newtype UnitV4 a = UnitV4 {unUnitV4 :: V4 a}  deriving (Show)

instance (Arbitrary a, Epsilon a, Floating a) => Arbitrary (UnitV4 a) where
  arbitrary = do
    v <- V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> (arbitrary `suchThat` (not . nearZero))
    return . UnitV4 . signorm $ v

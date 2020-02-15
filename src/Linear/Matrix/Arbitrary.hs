{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linear.Matrix.Arbitrary (
  InvertibleM33(..)
  , DiagM33(..)
  , AffineM44(..)
  , InvertibleAffineM44(..)
) where

import           Control.Lens

import           Linear.Epsilon
import           Linear.Matrix
import           Linear.Metric
import qualified Linear.V3           as V3
import qualified Linear.V4           as V4
import           Test.QuickCheck

import           Linear.V3.Arbitrary ()
import           Linear.V4.Arbitrary ()


-- | `Arbitrary InvertibleM33` instances are always invertible
newtype InvertibleM33 a = InvertibleM33 { unInvertibleM33 :: M33 a }  deriving (Show)

instance (Arbitrary a, Epsilon a, Floating a) => Arbitrary (InvertibleM33 a) where
  arbitrary = do
    v1 <- arbitrary `suchThat` (not . nearZero)
    v2 <- arbitrary `suchThat` (not . nearZero) `suchThat` (not . nearZero . dot v1)
    v3 <- arbitrary `suchThat` (not . nearZero) `suchThat` (not . nearZero . dot v1) `suchThat` (not . nearZero . dot v2)
    return . InvertibleM33 $ V3.V3 v1 v2 v3

-- | `Arbitrary DiagM33` instances only have non-zero diagonal entries (could still be zero)
newtype DiagM33 a = DiagM33 { unDiagM33 :: M33 a } deriving (Show)

instance (Arbitrary a, Num a) => Arbitrary (DiagM33 a) where
  arbitrary = do
    s1 <- arbitrary
    s2 <- arbitrary
    s3 <- arbitrary
    return . DiagM33 $ V3.V3
      (V3.V3 s1 0 0)
      (V3.V3 0 s2 0)
      (V3.V3 0 0 s3)

-- | `Arbitrary AffineM44` instances are affine M44 matrices (i.e. have [0,0,0,1] in last row)
newtype AffineM44 a = AffineM44 { unAffineM44 :: M44 a } deriving (Show)

instance (Arbitrary a, Num a) => Arbitrary (AffineM44 a) where
  arbitrary = do
    r1 <- arbitrary
    r2 <- arbitrary
    r3 <- arbitrary
    return . AffineM44 $ V4.V4 r1 r2 r3 (V4.V4 0 0 0 1)


-- | `Arbitrary InvertibleAffineM44` instances are invertible affine M44 matrices
newtype InvertibleAffineM44 a = InvertibleAffineM44 { unInvertibleAffineM44 :: M44 a } deriving (Show)

instance (Arbitrary a, Num a) => Arbitrary (InvertibleAffineM44 a) where
  arbitrary = do
    m33part <- m33_to_m44 <$> arbitrary
    trans <- arbitrary
    return . InvertibleAffineM44 $ set (V4._w . V4._w) 1 . set translation trans $ m33part

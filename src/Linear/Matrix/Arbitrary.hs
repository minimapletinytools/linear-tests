{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linear.Matrix.Arbitrary (
  InvertibleM33(..)
  , DiagM33(..)
) where

import           Linear.Epsilon
import           Linear.Matrix
import           Linear.Metric
import           Linear.V3
import           Test.QuickCheck

import           Linear.V3.Arbitrary


newtype InvertibleM33 a = InvertibleM33 { unInvertibleM33 :: M33 a }  deriving (Show)

instance (Arbitrary a, Epsilon a, Floating a) => Arbitrary (InvertibleM33 a) where
  arbitrary = do
    v1 <- arbitrary `suchThat` (not . nearZero)
    v2 <- arbitrary `suchThat` (not . nearZero) `suchThat` (not . nearZero . dot v1)
    v3 <- arbitrary `suchThat` (not . nearZero) `suchThat` (not . nearZero . dot v1) `suchThat` (not . nearZero . dot v2)
    return . InvertibleM33 $ V3 v1 v2 v3

newtype DiagM33 a = DiagM33 {unDiagM33 :: M33 a} deriving (Show)

instance (Arbitrary a, Num a) => Arbitrary (DiagM33 a) where
  arbitrary = do
    s1 <- arbitrary
    s2 <- arbitrary
    s3 <- arbitrary
    return . DiagM33 $ V3
      (V3 s1 0 0)
      (V3 0 s2 0)
      (V3 0 0 s3)

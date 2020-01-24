{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linear.Matrix.Arbitrary (
  InvertibleM33(..)
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

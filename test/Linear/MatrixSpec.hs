{-|
Module      : Linear.MatrixSpec
Description : property tests for 'Linear.Matrix' and 'Linear.Matrix.Arbitrary'
Maintainer  : chippermonky@gmail.com
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


module Linear.MatrixSpec (
  spec
) where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Lens

import           Linear.Epsilon
import qualified Linear.Matrix           as M
import qualified Linear.V3               as V3
import qualified Linear.V4               as V4

import           Linear.Matrix.Arbitrary


-- | test the property `m * m^-1 == identity`
prop_Matrix_InvertibleM33_isInvertible :: (Epsilon a, Floating a) => InvertibleM33 a -> Bool
prop_Matrix_InvertibleM33_isInvertible (InvertibleM33 m) = nearZero ((m M.!*! M.inv33 m ) - M.identity)

isDiagonal :: (Epsilon a) => M.M33 a -> Bool
isDiagonal m33 = all nearZero $ map (`view` m33) [
  V3._x . V3._y
  , V3._x . V3._z
  , V3._y . V3._x
  , V3._y . V3._z
  , V3._z . V3._x
  , V3._z . V3._y]

-- | test the property that all off diagonal entries are zero
prop_Matrix_DiagM33_isDiagonal :: (Epsilon a, Floating a) => DiagM33 a -> Bool
prop_Matrix_DiagM33_isDiagonal = isDiagonal . unDiagM33

isAffine :: (Epsilon a, Floating a) => M.M44 a -> Bool
isAffine m44 = nearZero $ view V4._w m44 - V4.V4 0 0 0 1

-- | test the property `m * m^-1 == identity`
prop_Matrix_InvertibleM44_isInvertible :: (Epsilon a, Floating a) => InvertibleM44 a -> Bool
prop_Matrix_InvertibleM44_isInvertible (InvertibleM44 m) = nearZero ((m M.!*! M.inv44 m ) - M.identity)

-- | test the property that last row is [0,0,0,1]
prop_Matrix_AffineM44_isAffine :: (Epsilon a, Floating a) => AffineM44 a -> Bool
prop_Matrix_AffineM44_isAffine (AffineM44 m44) = isAffine m44

-- | test that it is closed under multiplication
prop_Matrix_AffineM44_isClosedUnderMultiplication :: (Epsilon a, Floating a) => NonEmptyList (AffineM44 a) -> Bool
prop_Matrix_AffineM44_isClosedUnderMultiplication = isAffine . foldr1 (M.!*!) . map unAffineM44 . getNonEmpty

-- | test the property that last row is [0,0,0,1]
prop_Matrix_InvertibleAffineM44_isAffine :: (Epsilon a, Floating a) => InvertibleAffineM44 a -> Bool
prop_Matrix_InvertibleAffineM44_isAffine (InvertibleAffineM44 m44) = isAffine m44

-- | test the property `m * m^-1 == identity`
prop_Matrix_InvertibleAffineM44_isInvertible :: (Epsilon a, Floating a) => InvertibleAffineM44 a -> Bool
prop_Matrix_InvertibleAffineM44_isInvertible (InvertibleAffineM44 m44) = isAffine m44

spec :: Spec
spec = specTyped @Double

specTyped :: forall a. (Eq a, Show a, Arbitrary a, Epsilon a, Floating a) => Spec
specTyped = do
  describe "Matrix" $ do
    describe "M33" $ do
      describe "Arbitrary" $ do
        describe "InvertibleM33" $ do
          it "satifies property `m * m^-1 == identity`" $
            property $ prop_Matrix_InvertibleM33_isInvertible @a
        describe "DiagM33" $ do
          it "satisfies property that all off diagonal entries are zero" $
            property $ prop_Matrix_DiagM33_isDiagonal @a
    describe "M44" $ do
      describe "Arbitrary" $ do
        describe "InvertibleM44" $ do
          it "satifies property `m * m^-1 == identity`" $
            property $ prop_Matrix_InvertibleM44_isInvertible @a
        describe "AffineM44" $ do
          it "satisfies property [0,0,0,1] in last row" $
            property $ prop_Matrix_AffineM44_isAffine @a
          it "is closed under multiplication" $
            property $ prop_Matrix_AffineM44_isClosedUnderMultiplication @a
        describe "InvertibleAffineM44" $ do
          it "satisfies property [0,0,0,1] in last row" $
            property $ prop_Matrix_InvertibleAffineM44_isAffine @a
          it "satifies `m * m^-1 == identity`" $
            property $ prop_Matrix_InvertibleAffineM44_isInvertible @a

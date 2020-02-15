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
prop_Matrix_InvertibleM33_isInvertible :: InvertibleM33 Double -> Bool
prop_Matrix_InvertibleM33_isInvertible (InvertibleM33 m) = nearZero ((m M.!*! M.inv33 m ) - M.identity)

isDiagonal :: M.M33 Double -> Bool
isDiagonal m33 = all nearZero $ map (`view` m33) [
  V3._x . V3._y
  , V3._x . V3._z
  , V3._y . V3._x
  , V3._y . V3._z
  , V3._z . V3._x
  , V3._z . V3._y]

-- | test the property that all off diagonal entries are zero
prop_Matrix_DiagM33_isDiagonal :: DiagM33 Double -> Bool
prop_Matrix_DiagM33_isDiagonal = isDiagonal . unDiagM33

isAffine :: M.M44 Double -> Bool
isAffine m44 = nearZero $ view V4._w m44 - V4.V4 0 0 0 1

-- | test the property that last row is [0,0,0,1]
prop_Matrix_AffineM44_isAffine :: AffineM44 Double -> Bool
prop_Matrix_AffineM44_isAffine (AffineM44 m44) = isAffine m44

-- | test that it is closed under multiplication
prop_Matrix_AffineM44_isClosedUnderMultiplication :: NonEmptyList (AffineM44 Double) -> Bool
prop_Matrix_AffineM44_isClosedUnderMultiplication = isAffine . foldr1 (M.!*!) . map unAffineM44 . getNonEmpty

-- | test the property that last row is [0,0,0,1]
prop_Matrix_InvertibleAffineM44_isAffine :: InvertibleAffineM44 Double -> Bool
prop_Matrix_InvertibleAffineM44_isAffine (InvertibleAffineM44 m44) = isAffine m44

-- | test the property `m * m^-1 == identity`
prop_Matrix_InvertibleAffineM44_isInvertible :: InvertibleAffineM44 Double -> Bool
prop_Matrix_InvertibleAffineM44_isInvertible (InvertibleAffineM44 m44) = isAffine m44

spec :: Spec
spec = do
  describe "Matrix" $ do
    describe "M33" $ do
      describe "Arbitrary" $ do
        describe "InvertibleM33" $ do
          it "satifies property `m * m^-1 == identity`" $
            property prop_Matrix_InvertibleM33_isInvertible
        describe "DiagM33" $ do
          it "satisfies property that all off diagonal entries are zero" $
            property prop_Matrix_DiagM33_isDiagonal
    describe "M44" $ do
      describe "Arbitrary" $ do
        describe "AffineM44" $ do
          it "satisfies property [0,0,0,1] in last row" $
            property prop_Matrix_AffineM44_isAffine
          it "is closed under multiplication" $
            property prop_Matrix_AffineM44_isClosedUnderMultiplication
        describe "InvertibleAffineM44" $ do
          it "satisfies property [0,0,0,1] in last row" $
            property prop_Matrix_InvertibleAffineM44_isAffine
          it "satifies `m * m^-1 == identity`" $
            property prop_Matrix_InvertibleAffineM44_isInvertible

module Linear.MatrixSpec (
  spec
) where

import           Test.Hspec
import           Test.QuickCheck

import           Linear.Epsilon
import qualified Linear.Matrix           as M
import           Linear.Metric
import qualified Linear.Vector           as V

import           Linear.Matrix.Arbitrary
import           Linear.V3.Arbitrary

-- | test the property `m * m^-1 == identity`
prop_Matrix_InvertibleM33 :: InvertibleM33 Double -> Bool
prop_Matrix_InvertibleM33 (InvertibleM33 m) = nearZero ((m M.!*! M.inv33 m ) - M.identity)

-- | test the property `det m * v ⊗ v == 0`
-- i.e. the result of applying a diagonal matrix to vector is parallel to the input vector
prop_Matrix_DiagM33 :: DiagM33 Double -> UnitV3 Double -> Bool
prop_Matrix_DiagM33 (DiagM33 m) (UnitV3 v) = nearZero . M.det33 $ V.outer (m M.!* v) v

spec :: Spec
spec = do
  describe "Matrix" $ do
    describe "Arbitrary" $ do
      describe "InvertibleM33" $ do
        it "satifies `m * m^-1 == identity`" $
          property prop_Matrix_InvertibleM33
      describe "DiagM33" $ do
        it "satisfies `det m * v ⊗ v == 0`" $
          property prop_Matrix_DiagM33

{-# LANGUAGE TemplateHaskell #-}

import           Linear.Conjugate
import           Linear.Epsilon
import qualified Linear.Matrix               as M
import           Linear.Metric
import qualified Linear.Quaternion           as Q
import qualified Linear.V3                   as V
import qualified Linear.Vector               as V

import           Linear.Matrix.Arbitrary
import           Linear.Quaternion.Arbitrary
import           Linear.V3.Arbitrary
import           Test.QuickCheck

-- V3

prop_V3_UnitV3 :: UnitV3 Double -> Bool
prop_V3_UnitV3 (UnitV3 v) = nearZero $ norm v - 1

-- Matrix
-- | tests if InvertibleM33 is implmenented as expected
-- incidentally, also tests inv33
prop_Matrix_InvertibleM33 :: InvertibleM33 Double -> Bool
prop_Matrix_InvertibleM33 (InvertibleM33 m) = nearZero ((m M.!*! M.inv33 m ) - M.identity)

-- | tests if DiagM33 is implemented as expected
prop_Matrix_DiagM33 :: DiagM33 Double -> UnitV3 Double -> Bool
prop_Matrix_DiagM33 (DiagM33 m) (UnitV3 v) = nearZero . M.det33 $ V.outer (m M.!* v) v

-- Quaternion
-- | tests if conjugate property of Quaternion behaves as expected
prop_Quaternion_conjugate :: Q.Quaternion Double -> Bool
prop_Quaternion_conjugate q = nearZero $ distance (q * conjugate q) (Q.Quaternion 1 (V.V3 0 0 0))

-- | tests if the arbitrary method indeed returns a quaternion
prop_Quaternion_arbitrary :: Q.Quaternion Double -> Bool
prop_Quaternion_arbitrary q = nearZero $ norm q - 1




--Template haskell nonsense to run all properties prefixed with "prop_" in this file
return []

main :: IO Bool
main = $quickCheckAll
--main = $verboseCheckAll

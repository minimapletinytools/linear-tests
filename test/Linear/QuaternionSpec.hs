{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


module Linear.QuaternionSpec (
  spec
) where

import           Test.Hspec
import           Test.QuickCheck

import           Linear.Conjugate
import           Linear.Epsilon
import           Linear.Metric
import qualified Linear.Quaternion           as Q
import qualified Linear.V3                   as V

import           Linear.Quaternion.Arbitrary
import           Linear.V3.Arbitrary

-- | tests for the property `|(q*v)·v| = 1 or 0`
prop_CartesianQuaternion_arbitrary :: (Show a, Epsilon a, RealFloat a, Conjugate a) => CartesianQuaternion a -> CartesianUnitV3 a -> Bool
prop_CartesianQuaternion_arbitrary (CartesianQuaternion q) (CartesianUnitV3 v) = nearZero qvv || nearZero (qvv - 1) where
  qvv = abs $ Q.rotate q v `dot` v

-- | tests for the property `|q| = 1`
prop_Quaternion_arbitrary :: (Epsilon a, RealFloat a, Conjugate a) => Q.Quaternion a -> Bool
prop_Quaternion_arbitrary q = nearZero $ norm q - 1


-- | tests for the property `q * conjugate(q) = identity`
prop_Quaternion_conjugate :: (Epsilon a, RealFloat a, Conjugate a) => Q.Quaternion a -> Bool
prop_Quaternion_conjugate q = nearZero $ distance (q * conjugate q) (Q.Quaternion 1 (V.V3 0 0 0))


spec :: Spec
spec = specTyped @Double

specTyped :: forall a. (Eq a, Show a, Arbitrary a, Epsilon a, RealFloat a, Conjugate a) => Spec
specTyped = do
  describe "Quaternion" $ do
    describe "Arbitrary" $ do
      describe "Quaternion" $ do
        it "satisfies the property `|q| = 1`" $
          property $ prop_Quaternion_arbitrary @a
      describe "CartesianQuaternion" $ do
        it "satisfies the property `|(q*v)·v| = 1 or 0`" $
          property $ prop_CartesianQuaternion_arbitrary @a
    describe "Conjugate" $ do
      it "satisfies the property `q * conjugate(q) = identity`" $
        property $ prop_Quaternion_conjugate @a

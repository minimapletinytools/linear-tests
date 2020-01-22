module Linear.Arbitrary (
) where

import           Linear.V3
import           Test.QuickCheck

-- TODO move this into Linear.Arbitrary.V3 to allow others to declare their own instances
-- OR you could always require newtype wrapper.. but that's annoying
instance (Arbitrary a) => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

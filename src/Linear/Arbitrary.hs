{-|
Module      : Linear.Arbitrary
Description : 'Arbitrary' instances for data types in the linear package
Maintainer  : chippermonky@gmail.com
-}

module Linear.Arbitrary (
  module Linear.Matrix.Arbitrary,
  module Linear.Quaternion.Arbitrary,
  module Linear.V3.Arbitrary
) where

import           Linear.Matrix.Arbitrary
import           Linear.Quaternion.Arbitrary
import           Linear.V3.Arbitrary

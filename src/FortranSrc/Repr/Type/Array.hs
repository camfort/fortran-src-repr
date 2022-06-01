module FortranSrc.Repr.Type.Array where

import FortranSrc.Repr.Type.Scalar
import Numeric.Natural

import GHC.Generics ( Generic )
import Data.Data ( Data )

data FTA = FTA
  { ftaScalar :: FTS
  , ftaShape  :: Shape
  } deriving stock (Generic, Data, Show, Eq, Ord)

newtype Shape = Shape { getShape :: [Natural] }
    deriving stock (Generic, Data, Show, Eq, Ord)

ftaSize :: FTA -> Natural
ftaSize = sum . getShape . ftaShape

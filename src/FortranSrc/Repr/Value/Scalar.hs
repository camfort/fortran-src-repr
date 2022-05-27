module FortranSrc.Repr.Value.Scalar where

import FortranSrc.Repr.Value.Common
import FortranSrc.Repr.Value.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Complex
import FortranSrc.Repr.Value.Scalar.Logical
import FortranSrc.Repr.Value.Scalar.String
import GHC.Generics ( Generic )
import Data.Data ( Data )

data FV (pr :: PrimRepr)
  = FVInt     (SomeFInt     pr)
  | FVReal    SomeFReal
  | FVComplex SomeFComplex
  | FVLogical (SomeFLogical pr)
  | FVString  SomeFString
    deriving stock (Show, Eq)

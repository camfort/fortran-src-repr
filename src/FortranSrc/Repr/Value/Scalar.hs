module FortranSrc.Repr.Value.Scalar where

import FortranSrc.Repr.Value.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Complex
import FortranSrc.Repr.Value.Scalar.String
import GHC.Generics ( Generic )

data FVM
  = FVInt     (SomeFInt FIntM)
  | FVReal    SomeFReal
  | FVComplex SomeFComplex
  | FVLogical (SomeFInt FIntM)
  | FVString  SomeFString
    deriving stock (Generic, Show, Eq)

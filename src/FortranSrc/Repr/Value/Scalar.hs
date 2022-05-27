module FortranSrc.Repr.Value.Scalar where

import FortranSrc.Repr.Value.Common
import FortranSrc.Repr.Value.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Complex
import FortranSrc.Repr.Value.Scalar.Logical
import FortranSrc.Repr.Value.Scalar.String

data FV (pr :: PrimRepr)
  = FVInt     (SomeFInt     pr)
  | FVReal    (SomeFReal    pr)
  | FVComplex (SomeFComplex pr)
  | FVLogical (SomeFLogical pr)
  | FVString  (SomeFString  pr)
    deriving stock (Show)

module FortranSrc.Repr.Value.Scalar.Machine where

import FortranSrc.Repr.Value.Scalar.Common
import FortranSrc.Repr.Value.Scalar.Int.Machine
import FortranSrc.Repr.Value.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Complex
import FortranSrc.Repr.Value.Scalar.String
import FortranSrc.Repr.Type.Scalar
import GHC.Generics ( Generic )

-- | A Fortran scalar value.
data FScalarValue
  = FSVInt     SomeFInt
  | FSVReal    SomeFReal
  | FSVComplex SomeFComplex
  | FSVLogical SomeFInt
  | FSVString  SomeFString
    deriving stock (Generic, Show, Eq)

-- | Recover a Fortran scalar value's type.
fScalarValueType :: FScalarValue -> FScalarType
fScalarValueType = \case
  FSVInt     a -> FSTInt     $ someFKindedKind a
  FSVReal    a -> FSTReal    $ someFKindedKind a
  FSVComplex a -> FSTComplex $ someFKindedKind a
  FSVLogical a -> FSTLogical $ someFKindedKind a
  FSVString  a -> FSTString  $ someFStringLen  a

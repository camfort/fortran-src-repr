module FortranSrc.Repr.Value.Machine where

import FortranSrc.Repr.Value.Scalar.Machine
import FortranSrc.Repr.Value.Array.Machine
import FortranSrc.Repr.Type

-- | A Fortran value (scalar or array).
data FValue = MkFArrayValue FArrayValue | MkFScalarValue FScalarValue
    deriving stock Show

fValueType :: FValue -> FType
fValueType = \case
  MkFScalarValue a -> MkFScalarType $ fScalarValueType a
  MkFArrayValue  a -> MkFArrayType  $ fArrayValueType  a

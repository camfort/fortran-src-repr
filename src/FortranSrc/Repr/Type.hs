module FortranSrc.Repr.Type where

import FortranSrc.Repr.Type.Scalar
import FortranSrc.Repr.Type.Array

-- | A Fortran type (scalar or array).
data FType = MkFScalarType FScalarType | MkFArrayType FArrayType
    deriving stock Show

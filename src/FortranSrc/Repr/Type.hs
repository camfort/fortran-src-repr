module FortranSrc.Repr.Type where

import FortranSrc.Repr.Type.Scalar
import FortranSrc.Repr.Type.Array
import GHC.Generics ( Generic )

-- | A Fortran type (scalar or array).
data FType = MkFScalarType FScalarType | MkFArrayType FArrayType
    deriving stock (Generic, Eq, Show)

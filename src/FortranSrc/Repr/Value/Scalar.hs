module FortranSrc.Repr.Value.Scalar where

import FortranSrc.Repr.Value.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Complex
import FortranSrc.Repr.Value.Scalar.String
import FortranSrc.Repr.Type.Scalar
import FortranSrc.Repr.Type.Scalar.Complex
import GHC.Generics ( Generic )

data FVM
  = FVInt     (SomeFInt FIntM)
  | FVReal    SomeFReal
  | FVComplex SomeFComplex
  | FVLogical (SomeFInt FIntM)
  | FVString  SomeFString
    deriving stock (Generic, Show, Eq)

fvmType :: FVM -> FTS
fvmType = \case
  FVInt     a -> FTInt      $ someFIntKind     a
  FVReal    a -> FTReal     $ someFRealKind    a
  FVComplex a -> FTComplex' $ FTComplex $ someFComplexKind a
  FVLogical a -> FTLogical  $ someFIntKind     a
  FVString  a -> FTString   $ someFStringLen   a

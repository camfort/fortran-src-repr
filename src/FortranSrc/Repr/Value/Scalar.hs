module FortranSrc.Repr.Value.Scalar where

import FortranSrc.Repr.Value.Scalar.Common
import FortranSrc.Repr.Value.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Complex
import FortranSrc.Repr.Value.Scalar.String
import FortranSrc.Repr.Type.Scalar
import GHC.Generics ( Generic )

data FVM
  = FVInt     SomeFIntM
  | FVReal    SomeFReal
  | FVComplex SomeFComplex
  | FVLogical SomeFIntM
  | FVString  SomeFString
    deriving stock (Generic, Show, Eq)

fvmType :: FVM -> FTS
fvmType = \case
  FVInt     a -> FTInt     $ someFKindedKind a
  FVReal    a -> FTReal    $ someFKindedKind a
  FVComplex a -> FTComplex $ someFKindedKind a
  FVLogical a -> FTLogical $ someFKindedKind a
  FVString  a -> FTString  $ someFStringLen  a

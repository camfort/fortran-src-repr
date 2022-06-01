module FortranSrc.Repr.Value.Scalar where

import FortranSrc.Repr.Value.Scalar.Common
import FortranSrc.Repr.Value.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Complex
import FortranSrc.Repr.Value.Scalar.String
import FortranSrc.Repr.Type.Scalar
import GHC.Generics ( Generic )

data FVSM
  = FVSMInt     SomeFIntM
  | FVSMReal    SomeFReal
  | FVSMComplex SomeFComplex
  | FVSMLogical SomeFIntM
  | FVSMString  SomeFString
    deriving stock (Generic, Show, Eq)

fvmType :: FVSM -> FTS
fvmType = \case
  FVSMInt     a -> FTInt     $ someFKindedKind a
  FVSMReal    a -> FTReal    $ someFKindedKind a
  FVSMComplex a -> FTComplex $ someFKindedKind a
  FVSMLogical a -> FTLogical $ someFKindedKind a
  FVSMString  a -> FTString  $ someFStringLen  a

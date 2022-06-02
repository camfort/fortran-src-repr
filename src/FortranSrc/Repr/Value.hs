module FortranSrc.Repr.Value where

import FortranSrc.Repr.Value.Scalar
import FortranSrc.Repr.Value.Array
import FortranSrc.Repr.Type

data FVM = FVAM FVAM | FVSM FVSM
    deriving stock Show

fvmType :: FVM -> FT
fvmType = \case
  FVSM a -> FTS $ fvsmType a
  FVAM a -> FTA $ fvaType a

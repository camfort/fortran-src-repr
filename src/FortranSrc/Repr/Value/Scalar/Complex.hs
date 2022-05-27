module FortranSrc.Repr.Value.Scalar.Complex where

import FortranSrc.Repr.Value.Common
import FortranSrc.Repr.Type.Scalar.Complex
import FortranSrc.Repr.Type.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Real
import Data.Singletons

data FComplex (pr :: PrimRepr) (k :: FTComplex) where
    FComplex8  :: FReal pr 'FTReal4 -> FReal pr 'FTReal4 -> FComplex pr 'FTComplex8
    FComplex16 :: FReal pr 'FTReal8 -> FReal pr 'FTReal8 -> FComplex pr 'FTComplex16
deriving stock instance Show (FComplex pr k)

data SomeFComplex pr
  = forall (k :: FTComplex). SomeFComplex (Sing k) (FComplex pr k)
deriving stock instance Show (SomeFComplex pr)

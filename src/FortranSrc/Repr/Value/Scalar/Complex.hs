module FortranSrc.Repr.Value.Scalar.Complex where

import FortranSrc.Repr.Type.Scalar.Complex
import FortranSrc.Repr.Type.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Real
import Data.Singletons
import GHC.Float ( float2Double )

data FComplex (k :: FTComplex) where
    FComplex8  :: FReal 'FTReal4 -> FReal 'FTReal4 -> FComplex 'FTComplex8
    FComplex16 :: FReal 'FTReal8 -> FReal 'FTReal8 -> FComplex 'FTComplex16
deriving stock instance Show (FComplex k)
deriving stock instance Eq   (FComplex k)

data SomeFComplex = forall (k :: FTComplex). SomeFComplex (Sing k) (FComplex k)
deriving stock instance Show SomeFComplex
instance Eq SomeFComplex where
    (SomeFComplex _ c1) == (SomeFComplex _ c2) =
        case (c1, c2) of
          (FComplex8  r1 i1, FComplex8  r2 i2) -> r1 == r2 && i1 == i2
          (FComplex16 r1 i1, FComplex16 r2 i2) -> r1 == r2 && i1 == i2
          (FComplex8  (FReal4 r1) (FReal4 i1), FComplex16 (FReal8 r2) (FReal8 i2)) ->
              float2Double r1 == r2 && float2Double i1 == i2
          (FComplex16 (FReal8 r1) (FReal8 i1), FComplex8  (FReal4 r2) (FReal4 i2)) ->
              r1 == float2Double r2 && i1 == float2Double i2

-- TODO Ord instance

module FortranSrc.Repr.Value.Scalar.Complex where

import FortranSrc.Repr.Type.Scalar.Complex
import FortranSrc.Repr.Type.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Real
import GHC.Float ( float2Double )

data FComplex (k :: FTComplex) where
    FComplex8  :: Float  -> Float  -> FComplex 'FTComplex8
    FComplex16 :: Double -> Double -> FComplex 'FTComplex16
deriving stock instance Show (FComplex k)
deriving stock instance Eq   (FComplex k)

data SomeFComplex = forall (k :: FTComplex). SomeFComplex (FComplex k)
deriving stock instance Show SomeFComplex
instance Eq SomeFComplex where
    (SomeFComplex c1) == (SomeFComplex c2) =
        case (c1, c2) of
          (FComplex8  r1 i1, FComplex8  r2 i2) -> r1 == r2 && i1 == i2
          (FComplex16 r1 i1, FComplex16 r2 i2) -> r1 == r2 && i1 == i2
          (FComplex8  r1 i1, FComplex16 r2 i2) ->
              float2Double r1 == r2 && float2Double i1 == i2
          (FComplex16 r1 i1, FComplex8  r2 i2) ->
              r1 == float2Double r2 && i1 == float2Double i2

someFComplexRealBinOp
    :: (Float  -> Float  -> Float)
    -> (Double -> Double -> Double)
    -> SomeFReal -> SomeFComplex -> SomeFComplex
someFComplexRealBinOp k8f k16f (SomeFReal r) (SomeFComplex c) =
    case (r, c) of
      (FReal4 r, FComplex8  cr ci) ->
        SomeFComplex $ FComplex8  (k8f  cr r) ci
      (FReal8 r, FComplex16 cr ci) ->
        SomeFComplex $ FComplex16 (k16f cr r) ci
      (FReal4 r, FComplex16 cr ci) ->
        SomeFComplex $ FComplex16 (k16f cr (float2Double r)) ci
      (FReal8 r, FComplex8  cr ci) ->
        SomeFComplex $ FComplex16 (k16f (float2Double cr) r) (float2Double ci)

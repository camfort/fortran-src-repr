module FortranSrc.Repr.Value.Scalar.Real where

import FortranSrc.Repr.Type.Scalar.Real
import GHC.Float ( float2Double )

data FReal (k :: FTReal) where
    FReal4 :: Float  -> FReal 'FTReal4
    FReal8 :: Double -> FReal 'FTReal8
deriving stock instance Show (FReal k)
deriving stock instance Eq   (FReal k)
deriving stock instance Ord  (FReal k)

data SomeFReal = forall (k :: FTReal). SomeFReal (FReal k)
deriving stock instance Show SomeFReal
instance Eq  SomeFReal where (==) = someFRealBinOp (==) (==)
instance Ord SomeFReal where compare = someFRealBinOp compare compare

fRealBinOp
    :: (Float  -> Float  -> a)
    -> (Double -> Double -> a)
    -> FReal kl -> FReal kr -> a
fRealBinOp k4f k8f l r =
    case (l, r) of
      (FReal4 r1, FReal4 r2) -> k4f r1 r2
      (FReal8 r1, FReal8 r2) -> k8f r1 r2
      (FReal4 r1, FReal8 r2) -> k8f (float2Double r1) r2
      (FReal8 r1, FReal4 r2) -> k8f r1 (float2Double r2)

someFRealBinOp
    :: (Float  -> Float  -> a)
    -> (Double -> Double -> a)
    -> SomeFReal -> SomeFReal -> a
someFRealBinOp k4f k8f (SomeFReal l) (SomeFReal r) = fRealBinOp k4f k8f l r

someFRealAdd :: SomeFReal -> SomeFReal -> SomeFReal
someFRealAdd = someFRealBinOp (\l r -> SomeFReal $ FReal4 $ l+r)
                              (\l r -> SomeFReal $ FReal8 $ l+r)

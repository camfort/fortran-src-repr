module FortranSrc.Repr.Value.Scalar.Real where

import FortranSrc.Repr.Type.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Common
import GHC.Float ( float2Double )

data FReal (k :: FTReal) where
    FReal4 :: Float  -> FReal 'FTReal4
    FReal8 :: Double -> FReal 'FTReal8
deriving stock instance Show (FReal k)
deriving stock instance Eq   (FReal k)
deriving stock instance Ord  (FReal k)

fRealBOp'
    :: (Float  -> Float  -> r)
    -> (Double -> Double -> r)
    -> FReal kl -> FReal kr -> r
fRealBOp' k4f k8f l r =
    case (l, r) of
      (FReal4 lr, FReal4 rr) -> k4f lr rr
      (FReal8 lr, FReal8 rr) -> k8f lr rr
      (FReal4 lr, FReal8 rr) -> k8f (float2Double lr) rr
      (FReal8 lr, FReal4 rr) -> k8f lr (float2Double rr)

fRealBOp
    :: (forall a. RealFloat a => a -> a -> r)
    -> FReal kl -> FReal kr -> r
fRealBOp f = fRealBOp' f f

fRealUOp'
    :: (Float  -> r)
    -> (Double -> r)
    -> FReal k -> r
fRealUOp' k4f k8f = \case
  FReal4 r -> k4f r
  FReal8 r -> k8f r

fRealUOp
    :: (forall a. RealFloat a => a -> r)
    -> FReal k -> r
fRealUOp f = fRealUOp' f f

type SomeFReal = SomeFKinded FTReal FReal
deriving stock instance Show SomeFReal
instance Eq  SomeFReal where
    (SomeFKinded l) == (SomeFKinded r) = fRealBOp (==) l r
instance Ord SomeFReal where
    compare (SomeFKinded l) (SomeFKinded r) = fRealBOp compare l r

someFRealBOp'
    :: (Float  -> Float  -> r)
    -> (Double -> Double -> r)
    -> SomeFReal -> SomeFReal -> r
someFRealBOp' k4f k8f (SomeFKinded l) (SomeFKinded r) =
    fRealBOp' k4f k8f l             r

someFRealBOp
    :: (forall a. RealFloat a => a -> a -> r)
    -> SomeFReal -> SomeFReal -> r
someFRealBOp f = someFRealBOp' f f

someFRealBOpWrap'
    :: (Float  -> Float  -> Float)
    -> (Double -> Double -> Double)
    -> SomeFReal -> SomeFReal -> SomeFReal
someFRealBOpWrap' k4f  k8f =
    someFRealBOp' k4f' k8f'
  where
    k4f' l r = SomeFKinded $ FReal4 $ k4f l r
    k8f' l r = SomeFKinded $ FReal8 $ k8f l r

someFRealBOpWrap
    :: (forall a. RealFloat a => a -> a -> a)
    -> SomeFReal -> SomeFReal -> SomeFReal
someFRealBOpWrap f = someFRealBOpWrap' f f

someFRealUOp'
    :: (Float  -> r)
    -> (Double -> r)
    -> SomeFReal -> r
someFRealUOp' k4f k8f (SomeFKinded x) =
    fRealUOp' k4f k8f x

someFRealUOp
    :: (forall a. RealFloat a => a -> r)
    -> SomeFReal -> r
someFRealUOp f = someFRealUOp' f f

someFRealUOpWrap'
    :: (Float  -> Float)
    -> (Double -> Double)
    -> SomeFReal -> SomeFReal
someFRealUOpWrap' k4f  k8f =
    someFRealUOp' k4f' k8f'
  where
    k4f' = SomeFKinded . FReal4 . k4f
    k8f' = SomeFKinded . FReal8 . k8f

someFRealUOpWrap
    :: (forall a. RealFloat a => a -> a)
    -> SomeFReal -> SomeFReal
someFRealUOpWrap f = someFRealUOpWrap' f f

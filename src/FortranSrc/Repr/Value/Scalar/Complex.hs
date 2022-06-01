module FortranSrc.Repr.Value.Scalar.Complex where

import FortranSrc.Repr.Value.Scalar.Common
import FortranSrc.Repr.Type.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Real
import GHC.Float ( float2Double )

data FComplex (k :: FTReal) where
    FComplex8  :: Float  -> Float  -> FComplex 'FTReal4
    FComplex16 :: Double -> Double -> FComplex 'FTReal8
deriving stock instance Show (FComplex k)
deriving stock instance Eq   (FComplex k)
deriving stock instance Ord  (FComplex k) -- TODO

fComplexBOp'
    :: (Float  -> Float  -> a)
    -> (a -> a -> r)
    -> (Double -> Double -> b)
    -> (b -> b -> r)
    -> FComplex kl -> FComplex kr -> r
fComplexBOp' k8f k8g k16f k16g l r =
    case (l, r) of
      (FComplex8  lr li, FComplex8  rr ri) -> k8g  (k8f  lr rr) (k8f  li ri)
      (FComplex16 lr li, FComplex16 rr ri) -> k16g (k16f lr rr) (k16f li ri)
      (FComplex8  lr li, FComplex16 rr ri) ->
        let lr' = float2Double lr
            li' = float2Double li
        in  k16g (k16f lr' rr) (k16f li' ri)
      (FComplex16 lr li, FComplex8  rr ri) ->
        let rr' = float2Double rr
            ri' = float2Double ri
        in  k16g (k16f lr rr') (k16f li ri')

fComplexBOp
    :: (forall a. RealFloat a => a -> a -> b)
    -> (b -> b -> r)
    -> FComplex kl -> FComplex kr -> r
fComplexBOp f g = fComplexBOp' f g f g

type SomeFComplex = SomeFKinded FTReal FComplex
deriving stock instance Show SomeFComplex
instance Eq  SomeFComplex where
    (SomeFKinded l) == (SomeFKinded r) = fComplexBOp (==) (&&) l r

someFComplexBOp'
    :: (Float  -> Float  -> a)
    -> (a -> a -> r)
    -> (Double -> Double -> b)
    -> (b -> b -> r)
    -> SomeFComplex -> SomeFComplex -> r
someFComplexBOp' k8f k8g k16f k16g (SomeFKinded l) (SomeFKinded r) =
    fComplexBOp' k8f k8g k16f k16g l                r

someFComplexBOp
    :: (forall a. RealFloat a => a -> a -> b)
    -> (b -> b -> r)
    -> SomeFComplex -> SomeFComplex -> r
someFComplexBOp f g = someFComplexBOp' f g f g

someFComplexBOpWrap'
    :: (Float  -> Float  -> Float)
    -> (Double -> Double -> Double)
    -> SomeFComplex -> SomeFComplex -> SomeFComplex
someFComplexBOpWrap' k8f     k16f =
    someFComplexBOp' k8f k8g k16f k16g
  where
    k8g  l r = SomeFKinded $ FComplex8  l r
    k16g l r = SomeFKinded $ FComplex16 l r

someFComplexBOpWrap
    :: (forall a. RealFloat a => a -> a -> a)
    -> SomeFComplex -> SomeFComplex -> SomeFComplex
someFComplexBOpWrap f = someFComplexBOpWrap' f f

someFComplexFromReal :: SomeFReal -> SomeFComplex
someFComplexFromReal (SomeFKinded r) =
    case r of
      FReal4 x -> SomeFKinded $ FComplex8  x 0.0
      FReal8 x -> SomeFKinded $ FComplex16 x 0.0

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-} -- just for better inference (maybe)

module FortranSrc.Repr.Value.Scalar.Int where

import FortranSrc.Repr.Type.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Common
import Data.Kind
import Data.Int
import Data.Singletons

data FIntM (k :: FTInt) where
    FIntM1 :: FIntMRep 'FTInt1 -> FIntM 'FTInt1
    FIntM2 :: FIntMRep 'FTInt2 -> FIntM 'FTInt2
    FIntM4 :: FIntMRep 'FTInt4 -> FIntM 'FTInt4
    FIntM8 :: FIntMRep 'FTInt8 -> FIntM 'FTInt8
deriving stock instance Show (FIntM k)
deriving stock instance Eq   (FIntM k)
deriving stock instance Ord  (FIntM k)

-- because we want to use this information elsewhere too
type FIntMRep :: FTInt -> Type
type family FIntMRep k = r | r -> k where
    FIntMRep 'FTInt1 = Int8
    FIntMRep 'FTInt2 = Int16
    FIntMRep 'FTInt4 = Int32
    FIntMRep 'FTInt8 = Int64

fIntMUOp'
    :: (Int8  -> r)
    -> (Int16 -> r)
    -> (Int32 -> r)
    -> (Int64 -> r)
    -> FIntM k -> r
fIntMUOp' k1f k2f k4f k8f = \case
  FIntM4 i32 -> k4f i32
  FIntM8 i64 -> k8f i64
  FIntM2 i16 -> k2f i16
  FIntM1 i8  -> k1f i8

fIntMUOp
    :: (forall a. Integral a => a -> r)
    -> FIntM k -> r
fIntMUOp f = fIntMUOp' f f f f

withFIntM :: Num a => FIntM k -> a
withFIntM = fIntMUOp fromIntegral

-- pattern matches are ordered to match more common ops earlier
fIntMBOp'
    :: (Int8  -> Int8  -> r)
    -> (Int16 -> Int16 -> r)
    -> (Int32 -> Int32 -> r)
    -> (Int64 -> Int64 -> r)
    -> FIntM kl -> FIntM kr -> r
fIntMBOp' k1f k2f k4f k8f il ir = case (il, ir) of
  (FIntM4 l32, FIntM4 r32) -> k4f l32 r32
  (FIntM8 l64, FIntM8 r64) -> k8f l64 r64
  (FIntM8 l64, _) -> k8f l64 (withFIntM ir)
  (_, FIntM8 r64) -> k8f (withFIntM il) r64
  (FIntM4 l32, _) -> k4f l32 (withFIntM ir)
  (_, FIntM4 r32) -> k4f (withFIntM il) r32
  (FIntM2 l16, FIntM2 r16) -> k2f l16 r16
  (FIntM2 l16, _) -> k2f l16 (withFIntM ir)
  (_, FIntM2 r16) -> k2f (withFIntM il) r16
  (FIntM1 l8,  FIntM1 r8)  -> k1f l8  r8

fIntMBOp
    :: (forall a. Integral a => a -> a -> r)
    -> FIntM kl -> FIntM kr -> r
fIntMBOp f = fIntMBOp' f f f f

--------------------------------------------------------------------------------

type SomeFIntM = SomeFKinded FTInt FIntM
deriving stock instance Show SomeFIntM
instance Eq SomeFIntM where
    (SomeFKinded l) == (SomeFKinded r) = fIntMBOp (==) l r

someFIntMUOp'
    :: (Int8  -> r)
    -> (Int16 -> r)
    -> (Int32 -> r)
    -> (Int64 -> r)
    -> SomeFIntM -> r
someFIntMUOp' k1f k2f k4f k8f (SomeFKinded i) =
    fIntMUOp' k1f k2f k4f k8f i

someFIntMUOp
    :: (forall a. Integral a => a -> r)
    -> SomeFIntM -> r
someFIntMUOp f = someFIntMUOp' f f f f

someFIntMUOpWrap'
    :: (Int8  -> Int8)
    -> (Int16 -> Int16)
    -> (Int32 -> Int32)
    -> (Int64 -> Int64)
    -> SomeFIntM -> SomeFIntM
someFIntMUOpWrap' k1f  k2f  k4f  k8f  (SomeFKinded i) =
    fIntMUOp'     k1f' k2f' k4f' k8f' i
  where
    k1f' = SomeFKinded . FIntM1 . k1f
    k2f' = SomeFKinded . FIntM2 . k2f
    k4f' = SomeFKinded . FIntM4 . k4f
    k8f' = SomeFKinded . FIntM8 . k8f

someFIntMUOpWrap
    :: (forall a. Integral a => a -> a)
    -> SomeFIntM -> SomeFIntM
someFIntMUOpWrap f = someFIntMUOpWrap' f f f f

someFIntMBOp'
    :: (Int8  -> Int8  -> r)
    -> (Int16 -> Int16 -> r)
    -> (Int32 -> Int32 -> r)
    -> (Int64 -> Int64 -> r)
    -> SomeFIntM -> SomeFIntM -> r
someFIntMBOp' k1f k2f k4f k8f (SomeFKinded il) (SomeFKinded ir) =
    fIntMBOp' k1f k2f k4f k8f il            ir

someFIntMBOp
    :: (forall a. Integral a => a -> a -> r)
    -> SomeFIntM -> SomeFIntM -> r
someFIntMBOp f = someFIntMBOp' f f f f

someFIntMBOpWrap'
    :: (Int8  -> Int8  -> Int8)
    -> (Int16 -> Int16 -> Int16)
    -> (Int32 -> Int32 -> Int32)
    -> (Int64 -> Int64 -> Int64)
    -> SomeFIntM -> SomeFIntM -> SomeFIntM
someFIntMBOpWrap' k1f  k2f  k4f  k8f =
    someFIntMBOp' k1f' k2f' k4f' k8f'
  where
    k1f' l r = SomeFKinded $ FIntM1 $ k1f l r
    k2f' l r = SomeFKinded $ FIntM2 $ k2f l r
    k4f' l r = SomeFKinded $ FIntM4 $ k4f l r
    k8f' l r = SomeFKinded $ FIntM8 $ k8f l r

someFIntMBOpWrap
    :: (forall a. Integral a => a -> a -> a)
    -> SomeFIntM -> SomeFIntM -> SomeFIntM
someFIntMBOpWrap f = someFIntMBOpWrap' f f f f

--------------------------------------------------------------------------------

newtype FIntI (k :: FTInt) = FIntI Integer
    deriving (Show, Eq, Ord) via Integer

--------------------------------------------------------------------------------

fIntICheckBounds
    :: forall k rep. (rep ~ FIntMRep k, Bounded rep, Integral rep)
    => FIntI k -> Maybe String
fIntICheckBounds (FIntI i) =
    if   i > fromIntegral (maxBound @rep)
    then Just "TODO too large"
    else if 　i < fromIntegral (minBound @rep)
         then Just "TODO too small"
         else Nothing

type SomeFIntI = SomeFKinded FTInt FIntI
deriving stock instance Show SomeFIntI
instance Eq SomeFIntI where
    (SomeFKinded (FIntI l)) == (SomeFKinded (FIntI r)) = l == r

-- this might look silly, but it's because even if we don't do kinded
-- calculations, we must still kind the output
someFIntIBOpWrap
    :: (Integer -> Integer -> Integer)
    -> SomeFIntI -> SomeFIntI -> SomeFIntI
someFIntIBOpWrap f l@(SomeFKinded (FIntI il)) r@(SomeFKinded (FIntI ir)) =
    case (someFKindedKind l, someFKindedKind r) of
      (FTInt8, _) -> as @'FTInt8
      (_, FTInt8) -> as @'FTInt8
      (FTInt4, _) -> as @'FTInt4
      (_, FTInt4) -> as @'FTInt4
      (FTInt2, _) -> as @'FTInt2
      (_, FTInt2) -> as @'FTInt2
      (FTInt1, FTInt1) -> as @'FTInt1
  where
    x = f il ir
    as :: forall (k :: FTInt). SingI k => SomeFIntI
    as = SomeFKinded $ FIntI @k x

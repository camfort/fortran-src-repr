module FortranSrc.Repr.Value.Scalar.Int.Machine where

import FortranSrc.Repr.Type.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Common
import Data.Int

data FInt (k :: FTInt) where
    FInt1 :: Int8  -> FInt 'FTInt1
    FInt2 :: Int16 -> FInt 'FTInt2
    FInt4 :: Int32 -> FInt 'FTInt4
    FInt8 :: Int64 -> FInt 'FTInt8
deriving stock instance Show (FInt k)
deriving stock instance Eq   (FInt k)
deriving stock instance Ord  (FInt k)

fIntUOp'
    :: (Int8  -> r)
    -> (Int16 -> r)
    -> (Int32 -> r)
    -> (Int64 -> r)
    -> FInt k -> r
fIntUOp' k1f k2f k4f k8f = \case
  FInt4 i32 -> k4f i32
  FInt8 i64 -> k8f i64
  FInt2 i16 -> k2f i16
  FInt1 i8  -> k1f i8

fIntUOp
    :: (forall a. Integral a => a -> r)
    -> FInt k -> r
fIntUOp f = fIntUOp' f f f f

withFInt :: Num a => FInt k -> a
withFInt = fIntUOp fromIntegral

-- pattern matches are ordered to match more common ops earlier
fIntBOp'
    :: (Int8  -> Int8  -> r)
    -> (Int16 -> Int16 -> r)
    -> (Int32 -> Int32 -> r)
    -> (Int64 -> Int64 -> r)
    -> FInt kl -> FInt kr -> r
fIntBOp' k1f k2f k4f k8f il ir = case (il, ir) of
  (FInt4 l32, FInt4 r32) -> k4f l32 r32
  (FInt8 l64, FInt8 r64) -> k8f l64 r64
  (FInt8 l64, _) -> k8f l64 (withFInt ir)
  (_, FInt8 r64) -> k8f (withFInt il) r64
  (FInt4 l32, _) -> k4f l32 (withFInt ir)
  (_, FInt4 r32) -> k4f (withFInt il) r32
  (FInt2 l16, FInt2 r16) -> k2f l16 r16
  (FInt2 l16, _) -> k2f l16 (withFInt ir)
  (_, FInt2 r16) -> k2f (withFInt il) r16
  (FInt1 l8,  FInt1 r8)  -> k1f l8  r8

fIntBOp
    :: (forall a. Integral a => a -> a -> r)
    -> FInt kl -> FInt kr -> r
fIntBOp f = fIntBOp' f f f f

--------------------------------------------------------------------------------

type SomeFInt = SomeFKinded FTInt FInt
deriving stock instance Show SomeFInt
instance Eq SomeFInt where
    (SomeFKinded l) == (SomeFKinded r) = fIntBOp (==) l r

someFIntUOp'
    :: (Int8  -> r)
    -> (Int16 -> r)
    -> (Int32 -> r)
    -> (Int64 -> r)
    -> SomeFInt -> r
someFIntUOp' k1f k2f k4f k8f (SomeFKinded i) =
    fIntUOp' k1f k2f k4f k8f i

someFIntUOp
    :: (forall a. Integral a => a -> r)
    -> SomeFInt -> r
someFIntUOp f = someFIntUOp' f f f f

someFIntUOpWrap'
    :: (Int8  -> Int8)
    -> (Int16 -> Int16)
    -> (Int32 -> Int32)
    -> (Int64 -> Int64)
    -> SomeFInt -> SomeFInt
someFIntUOpWrap' k1f  k2f  k4f  k8f  (SomeFKinded i) =
    fIntUOp'     k1f' k2f' k4f' k8f' i
  where
    k1f' = SomeFKinded . FInt1 . k1f
    k2f' = SomeFKinded . FInt2 . k2f
    k4f' = SomeFKinded . FInt4 . k4f
    k8f' = SomeFKinded . FInt8 . k8f

someFIntUOpWrap
    :: (forall a. Integral a => a -> a)
    -> SomeFInt -> SomeFInt
someFIntUOpWrap f = someFIntUOpWrap' f f f f

someFIntBOp'
    :: (Int8  -> Int8  -> r)
    -> (Int16 -> Int16 -> r)
    -> (Int32 -> Int32 -> r)
    -> (Int64 -> Int64 -> r)
    -> SomeFInt -> SomeFInt -> r
someFIntBOp' k1f k2f k4f k8f (SomeFKinded il) (SomeFKinded ir) =
    fIntBOp' k1f k2f k4f k8f il            ir

someFIntBOp
    :: (forall a. Integral a => a -> a -> r)
    -> SomeFInt -> SomeFInt -> r
someFIntBOp f = someFIntBOp' f f f f

someFIntBOpWrap'
    :: (Int8  -> Int8  -> Int8)
    -> (Int16 -> Int16 -> Int16)
    -> (Int32 -> Int32 -> Int32)
    -> (Int64 -> Int64 -> Int64)
    -> SomeFInt -> SomeFInt -> SomeFInt
someFIntBOpWrap' k1f  k2f  k4f  k8f =
    someFIntBOp' k1f' k2f' k4f' k8f'
  where
    k1f' l r = SomeFKinded $ FInt1 $ k1f l r
    k2f' l r = SomeFKinded $ FInt2 $ k2f l r
    k4f' l r = SomeFKinded $ FInt4 $ k4f l r
    k8f' l r = SomeFKinded $ FInt8 $ k8f l r

someFIntBOpWrap
    :: (forall a. Integral a => a -> a -> a)
    -> SomeFInt -> SomeFInt -> SomeFInt
someFIntBOpWrap f = someFIntBOpWrap' f f f f

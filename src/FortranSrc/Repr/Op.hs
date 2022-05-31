module FortranSrc.Repr.Op where

import FortranSrc.Repr.Value.Scalar
import FortranSrc.Repr.Value.Scalar.Complex
import FortranSrc.Repr.Value.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Int
import FortranSrc.Repr.Type.Scalar.Real
import GHC.Float ( float2Double )

data Error
  = EBadArgType String
    deriving stock (Show, Eq)

-- https://gcc.gnu.org/onlinedocs/gfortran/DBLE.html#DBLE
opIcDble :: FVM -> Either Error (FReal 'FTReal8)
opIcDble = \case
  FVComplex (SomeFComplex c) -> case c of
    FComplex8  r _i -> Right $ FReal8 $ float2Double r
    FComplex16 r _i -> Right $ FReal8 r
  FVReal (SomeFReal r) -> case r of
    FReal4 r'   -> Right $ FReal8 $ float2Double r'
    FReal8 _r'  -> Right r
  FVInt (SomeFInt i) -> Right $ FReal8 $ withFIntM i
  _ -> Left $ EBadArgType "expected complex, real, int"

opIcNumericBOp
    :: (forall a. Num a => a -> a -> a)
    -> FVM -> FVM -> Either String FVM
opIcNumericBOp bop = go
  where
    go (FVInt l) (FVInt r) = Right $ FVInt $ someFIntMBOpWrap bop l r
    go (FVInt (SomeFInt l)) (FVReal r) =
        Right $ FVReal $ someFRealUOpWrap (\x -> withFIntM l `bop` x) r
    -- TODO int complex
    go (FVReal l) (FVReal r) = Right $ FVReal $ someFRealBOpWrap bop l r
    go (FVReal l) (FVInt r) = go (FVInt r) (FVReal l)
    go (FVReal l) (FVComplex r) =
        Right $ FVComplex $ someFComplexBOpWrap bop (someFComplexFromReal l) r

{-
opIcRelOp
    :: (forall a. Ord a => a -> a -> Bool)
    -> FVM -> FVM -> Either String Bool
opIcRelOp rop = go
  where
    go (FVInt l) (FVInt r) = Right $ someFIntMBOp rop l r
-}

opIcNumRelOp
    :: (forall a. Ord a => a -> a -> r)
    -> FVM -> FVM -> Either String r
opIcNumRelOp rop = go
  where
    go (FVInt l) (FVInt r) = Right $ someFIntMBOp rop l r
    go (FVInt (SomeFInt l)) (FVReal r) =
        Right $ someFRealUOp (\x -> withFIntM l `rop` x) r
    -- TODO int complex
    go (FVReal l) (FVReal r) = Right $ someFRealBOp rop l r
    go (FVReal l) (FVInt r) = go (FVInt r) (FVReal l)
    -- TODO real complex
    --go (FVString l) (FVString r) = _

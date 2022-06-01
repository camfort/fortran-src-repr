module FortranSrc.Repr.Op where

import FortranSrc.Repr.Value.Scalar
import FortranSrc.Repr.Value.Scalar.Common
import FortranSrc.Repr.Value.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Complex
import FortranSrc.Repr.Value.Scalar.String
import FortranSrc.Repr.Type.Scalar
import FortranSrc.Repr.Type.Scalar.Real
import GHC.Float ( float2Double )

data Error
  = EBadArgType1 [String] FTS
  | EBadArgType2 [String] FTS FTS
    deriving stock (Show, Eq)

-- https://gcc.gnu.org/onlinedocs/gfortran/DBLE.html#DBLE
opIcDble :: FVM -> Either Error (FReal 'FTReal8)
opIcDble = \case
  FVComplex (SomeFKinded c) -> case c of
    FComplex8  r _i -> rfr8 $ float2Double r
    FComplex16 r _i -> rfr8 r
  FVReal (SomeFKinded r) -> case r of
    FReal4 r'   -> rfr8 $ float2Double r'
    FReal8 _r'  -> Right r
  FVInt (SomeFKinded i) -> rfr8 $ withFIntM i
  v -> eBadArgType1 ["COMPLEX", "REAL", "INT"] v
  where rfr8 = Right . FReal8

eBadArgType1 :: [String] -> FVM -> Either Error a
eBadArgType1 expected got = Left $ EBadArgType1 expected $ fvmType got

eBadArgType2 :: [String] -> FVM -> FVM -> Either Error a
eBadArgType2 expected l r = Left $ EBadArgType2 expected (fvmType l) (fvmType r)

opIcNumericBOp
    :: (forall a. Num a => a -> a -> a)
    -> FVM -> FVM -> Either String FVM
opIcNumericBOp bop = go
  where
    go (FVInt l) (FVInt r) = Right $ FVInt $ someFIntMBOpWrap bop l r
    go (FVInt (SomeFKinded l)) (FVReal r) =
        Right $ FVReal $ someFRealUOpWrap (\x -> withFIntM l `bop` x) r
    -- TODO int complex
    go (FVReal l) (FVReal r) = Right $ FVReal $ someFRealBOpWrap bop l r
    go (FVReal l) (FVInt r) = go (FVInt r) (FVReal l)
    go (FVReal l) (FVComplex r) =
        Right $ FVComplex $ someFComplexBOpWrap bop (someFComplexFromReal l) r

opIcNumRelBOp
    :: (forall a. Ord a => a -> a -> r)
    -> FVM -> FVM -> Either String r
opIcNumRelBOp bop = go
  where
    go (FVInt l) (FVInt r) = Right $ someFIntMBOp bop l r
    go (FVInt (SomeFKinded l)) (FVReal r) =
        Right $ someFRealUOp (\x -> withFIntM l `bop` x) r
    -- TODO int complex
    go (FVReal l) (FVReal r) = Right $ someFRealBOp bop l r
    go (FVReal l) (FVInt r) = go (FVInt r) (FVReal l)
    -- TODO real complex
    go (FVString l) (FVString r) = Right $ someFStringBOp bop l r

-- not
opIcLogicalUOp
    :: (forall a. Eq a => a -> r)
    -> FVM -> Either Error r
opIcLogicalUOp uop = go
  where
    go = \case
      FVLogical a -> Right $ uop a
      v -> eBadArgType1 ["LOGICAL"] v

-- and, or, eqv, neqv
opIcLogicalBOp
    :: (forall a. Eq a => a -> a -> r)
    -> FVM -> FVM -> Either Error r
opIcLogicalBOp bop = go
  where
    go (FVLogical l) (FVLogical r) = Right $ bop l r
    go l r = eBadArgType2 ["LOGICAL"] l r

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
opIcDble :: FVSM -> Either Error (FReal 'FTReal8)
opIcDble = \case
  FVSMComplex (SomeFKinded c) -> case c of
    FComplex8  r _i -> rfr8 $ float2Double r
    FComplex16 r _i -> rfr8 r
  FVSMReal (SomeFKinded r) -> case r of
    FReal4 r'   -> rfr8 $ float2Double r'
    FReal8 _r'  -> Right r
  FVSMInt (SomeFKinded i) -> rfr8 $ withFIntM i
  v -> eBadArgType1 ["COMPLEX", "REAL", "INT"] v
  where rfr8 = Right . FReal8

eBadArgType1 :: [String] -> FVSM -> Either Error a
eBadArgType1 expected got = Left $ EBadArgType1 expected $ fvsmType got

eBadArgType2 :: [String] -> FVSM -> FVSM -> Either Error a
eBadArgType2 expected l r = Left $ EBadArgType2 expected (fvsmType l) (fvsmType r)

opIcNumericBOp
    :: (forall a. Num a => a -> a -> a)
    -> FVSM -> FVSM -> Either String FVSM
opIcNumericBOp bop = go
  where
    go (FVSMInt l) (FVSMInt r) = Right $ FVSMInt $ someFIntMBOpWrap bop l r
    go (FVSMInt (SomeFKinded l)) (FVSMReal r) =
        Right $ FVSMReal $ someFRealUOpWrap (\x -> withFIntM l `bop` x) r
    -- TODO int complex
    go (FVSMReal l) (FVSMReal r) = Right $ FVSMReal $ someFRealBOpWrap bop l r
    go (FVSMReal l) (FVSMInt r) = go (FVSMInt r) (FVSMReal l)
    go (FVSMReal l) (FVSMComplex r) =
        Right $ FVSMComplex $ someFComplexBOpWrap bop (someFComplexFromReal l) r

opIcNumRelBOp
    :: (forall a. Ord a => a -> a -> r)
    -> FVSM -> FVSM -> Either String r
opIcNumRelBOp bop = go
  where
    go (FVSMInt l) (FVSMInt r) = Right $ someFIntMBOp bop l r
    go (FVSMInt (SomeFKinded l)) (FVSMReal r) =
        Right $ someFRealUOp (\x -> withFIntM l `bop` x) r
    -- TODO int complex
    go (FVSMReal l) (FVSMReal r) = Right $ someFRealBOp bop l r
    go (FVSMReal l) (FVSMInt r) = go (FVSMInt r) (FVSMReal l)
    -- TODO real complex
    go (FVSMString l) (FVSMString r) = Right $ someFStringBOp bop l r

-- not
opIcLogicalUOp
    :: (forall a. Eq a => a -> r)
    -> FVSM -> Either Error r
opIcLogicalUOp uop = go
  where
    go = \case
      FVSMLogical a -> Right $ uop a
      v -> eBadArgType1 ["LOGICAL"] v

-- and, or, eqv, neqv
opIcLogicalBOp
    :: (forall a. Eq a => a -> a -> r)
    -> FVSM -> FVSM -> Either Error r
opIcLogicalBOp bop = go
  where
    go (FVSMLogical l) (FVSMLogical r) = Right $ bop l r
    go l r = eBadArgType2 ["LOGICAL"] l r

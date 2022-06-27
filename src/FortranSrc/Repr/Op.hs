module FortranSrc.Repr.Op where

import FortranSrc.Repr.Value.Scalar.Machine
import FortranSrc.Repr.Value.Scalar.Common
import FortranSrc.Repr.Value.Scalar.Int.Machine
import FortranSrc.Repr.Value.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Complex
import FortranSrc.Repr.Value.Scalar.String
import FortranSrc.Repr.Type.Scalar
import FortranSrc.Repr.Type.Scalar.Real
import GHC.Float ( float2Double )

data Error
  = EBadArgType1 [String] FScalarType
  | EBadArgType2 [String] FScalarType FScalarType
    deriving stock (Show, Eq)

-- https://gcc.gnu.org/onlinedocs/gfortran/DBLE.html#DBLE
opIcDble :: FScalarValue -> Either Error (FReal 'FTReal8)
opIcDble = \case
  FSVComplex (SomeFKinded c) -> case c of
    FComplex8  r _i -> rfr8 $ float2Double r
    FComplex16 r _i -> rfr8 r
  FSVReal (SomeFKinded r) -> case r of
    FReal4 r'   -> rfr8 $ float2Double r'
    FReal8 _r'  -> Right r
  FSVInt (SomeFKinded i) -> rfr8 $ withFInt i
  v -> eBadArgType1 ["COMPLEX", "REAL", "INT"] v
  where rfr8 = Right . FReal8

eBadArgType1 :: [String] -> FScalarValue -> Either Error a
eBadArgType1 expected = Left . EBadArgType1 expected . fScalarValueType

eBadArgType2 :: [String] -> FScalarValue -> FScalarValue -> Either Error a
eBadArgType2 expected l r =
    Left $ EBadArgType2 expected (fScalarValueType l) (fScalarValueType r)

opIcNumericBOp
    :: (forall a. Num a => a -> a -> a)
    -> FScalarValue -> FScalarValue -> Either Error FScalarValue
opIcNumericBOp bop = go
  where
    go (FSVInt l) (FSVInt r) = Right $ FSVInt $ someFIntBOpWrap bop l r
    go (FSVInt (SomeFKinded l)) (FSVReal r) =
        Right $ FSVReal $ someFRealUOpWrap (\x -> withFInt l `bop` x) r
    -- TODO int complex
    go (FSVReal l) (FSVReal r) = Right $ FSVReal $ someFRealBOpWrap bop l r
    go (FSVReal l) (FSVInt r) = go (FSVInt r) (FSVReal l)
    go (FSVReal l) (FSVComplex r) =
        Right $ FSVComplex $ someFComplexBOpWrap bop (someFComplexFromReal l) r

opIcNumRelBOp
    :: (forall a. Ord a => a -> a -> r)
    -> FScalarValue -> FScalarValue -> Either Error r
opIcNumRelBOp bop = go
  where
    go (FSVInt l) (FSVInt r) = Right $ someFIntBOp bop l r
    go (FSVInt (SomeFKinded l)) (FSVReal r) =
        Right $ someFRealUOp (\x -> withFInt l `bop` x) r
    -- TODO int complex
    go (FSVReal l) (FSVReal r) = Right $ someFRealBOp bop l r
    go (FSVReal l) (FSVInt r) = go (FSVInt r) (FSVReal l)
    -- TODO real complex
    go (FSVString l) (FSVString r) = Right $ someFStringBOp bop l r

-- not
opIcLogicalUOp
    :: (forall a. Eq a => a -> r)
    -> FScalarValue -> Either Error r
opIcLogicalUOp uop = go
  where
    go = \case
      FSVLogical a -> Right $ uop a
      v -> eBadArgType1 ["LOGICAL"] v

-- and, or, eqv, neqv
opIcLogicalBOp
    :: (forall a. Eq a => a -> a -> r)
    -> FScalarValue -> FScalarValue -> Either Error r
opIcLogicalBOp bop = go
  where
    go (FSVLogical l) (FSVLogical r) = Right $ bop l r
    go l r = eBadArgType2 ["LOGICAL"] l r

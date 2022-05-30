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
opIcDble :: FV pr -> Either Error (FReal 'FTReal8)
opIcDble = \case
  FVComplex (SomeFComplex c) -> case c of
    FComplex8  r _i -> Right $ FReal8 $ float2Double r
    FComplex16 r _i -> Right $ FReal8 r
  FVReal (SomeFReal r) -> case r of
    FReal4 r'   -> Right $ FReal8 $ float2Double r'
    FReal8 _r'  -> Right r
  FVInt (SomeFInt _ i) -> case i of
    FIntM i' -> Right $ FReal8 $ fromIntegral i'
    FIntI i' -> Right $ FReal8 $ fromIntegral i'
  _ -> Left $ EBadArgType "expected complex, real, int"

opIcPlus :: FV pr -> FV pr -> Either String (FV pr)
opIcPlus (FVInt l) (FVInt r) = Right $ FVInt $ fIntAdd l r
opIcPlus (FVInt l) (FVReal (SomeFReal r)) =
    case r of
      FReal4 r' -> Right $ FVReal $ SomeFReal $ FReal4 $ r' + fIntUse fromIntegral l
      FReal8 r' -> Right $ FVReal $ SomeFReal $ FReal8 $ r' + fIntUse fromIntegral l
opIcPlus (FVReal l) (FVReal r) = Right $ FVReal $ someFRealAdd l r
opIcPlus (FVReal l) (FVInt r) = opIcPlus (FVInt r) (FVReal l)
opIcPlus (FVReal l) (FVComplex r) = Right $ FVComplex $ someFComplexRealBinOp (+) (+) l r

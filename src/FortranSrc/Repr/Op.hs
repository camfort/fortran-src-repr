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
opIcDble :: FV pr -> Either Error (FReal pr 'FTReal8)
opIcDble = \case
  FVComplex (SomeFComplex _ c) -> case c of
    FComplex8 (FReal4 r) _i -> Right $ FReal8 $ float2Double r
    FComplex16 r _i -> Right r
  FVReal (SomeFReal _ r) -> case r of
    FReal4 r'   -> Right $ FReal8 $ float2Double r'
    FReal8 _r'  -> Right r
  FVInt (SomeFInt _ i) -> case i of
    FIntM i' -> Right $ FReal8 $ fromIntegral i'
    FIntI i' -> Right $ FReal8 $ fromIntegral i'
  _ -> Left $ EBadArgType "expected complex, real, int"

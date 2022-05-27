module FortranSrc.Repr.Op where

import FortranSrc.Repr.Value.Scalar
import FortranSrc.Repr.Value.Scalar.Complex
import FortranSrc.Repr.Value.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Int
import FortranSrc.Repr.Type.Scalar.Real
import GHC.Float ( float2Double )

opIcDble :: FV pr -> Maybe (FReal pr 'FTReal8)
opIcDble = \case
  FVComplex (SomeFComplex _ c) -> case c of
    FComplex8 (FReal4 r) _i -> Just $ FReal8 $ float2Double r
    FComplex16 r _i -> Just r
  FVReal (SomeFReal _ r) -> case r of
    FReal4 r' -> Just $ FReal8 $ float2Double r'
    FReal8 r' -> Just $ FReal8 r'
  FVInt (SomeFInt _ i) -> case i of
    FIntM i' -> Just $ FReal8 $ fromIntegral i'
    FIntI i' -> Just $ FReal8 $ fromIntegral i'
  _ -> Nothing

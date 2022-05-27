module FortranSrc.Repr.Value.Scalar.Real where

import FortranSrc.Repr.Type.Scalar.Real
import Data.Singletons
import GHC.Float ( float2Double )

data FReal (k :: FTReal) where
    FReal4 :: Float  -> FReal 'FTReal4
    FReal8 :: Double -> FReal 'FTReal8
deriving stock instance Show (FReal k)
deriving stock instance Eq   (FReal k)

data SomeFReal= forall (k :: FTReal). SomeFReal (Sing k) (FReal k)
deriving stock instance Show SomeFReal
instance Eq SomeFReal where
    (SomeFReal _ x1) == (SomeFReal _ x2) =
        case (x1, x2) of
          (FReal4 r1, FReal4 r2) -> r1 == r2
          (FReal8 r1, FReal8 r2) -> r1 == r2
          (FReal4 r1, FReal8 r2) -> float2Double r1 == r2
          (FReal8 r1, FReal4 r2) -> r1 == float2Double r2

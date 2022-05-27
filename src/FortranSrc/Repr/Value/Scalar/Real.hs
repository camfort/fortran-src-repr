module FortranSrc.Repr.Value.Scalar.Real where

import FortranSrc.Repr.Value.Common
import FortranSrc.Repr.Type.Scalar.Real
import Data.Singletons
import GHC.Float ( float2Double )

data FReal (pr :: PrimRepr) (k :: FTReal) where
    FReal4 :: Float  -> FReal pr 'FTReal4
    FReal8 :: Double -> FReal pr 'FTReal8
deriving stock instance Show (FReal pr k)
deriving stock instance Eq   (FReal pr k)

data SomeFReal pr
  = forall (k :: FTReal). SomeFReal (Sing k) (FReal pr k)
deriving stock instance Show (SomeFReal pr)
instance Eq (SomeFReal pr) where
    (SomeFReal _ x1) == (SomeFReal _ x2) =
        case (x1, x2) of
          (FReal4 r1, FReal4 r2) -> r1 == r2
          (FReal8 r1, FReal8 r2) -> r1 == r2
          (FReal4 r1, FReal8 r2) -> float2Double r1 == r2
          (FReal8 r1, FReal4 r2) -> r1 == float2Double r2

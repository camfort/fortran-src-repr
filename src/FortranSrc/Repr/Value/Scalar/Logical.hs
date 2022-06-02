module FortranSrc.Repr.Value.Scalar.Logical where

import FortranSrc.Repr.Type.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Int

newtype FLogicalI (k :: FTInt) = FLogicalI Bool
    deriving stock (Show, Eq, Ord)

someFLogicalMNot :: SomeFIntM -> SomeFIntM
someFLogicalMNot = someFIntMUOpWrap $ \bi -> if bi == 1 then 0 else 1

fLogicalM :: Bool -> FIntM 'FTInt4
fLogicalM = \case True -> FIntM4 1; False -> FIntM4 0

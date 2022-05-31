module FortranSrc.Repr.Value.Scalar.Logical where

import FortranSrc.Repr.Type.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Int

newtype FLogicalI (k :: FTInt) = FLogicalI Bool
    deriving stock (Show, Eq, Ord)

someFLogicalMNot :: SomeFInt FIntM -> SomeFInt FIntM
someFLogicalMNot = someFIntMUOpWrap $ \bi -> if bi == 1 then 0 else 1

module FortranSrc.Repr.Value.Scalar.Logical.Idealized where

import FortranSrc.Repr.Type.Scalar.Int

newtype FLogical (k :: FTInt) = FLogical Bool
    deriving stock (Show, Eq, Ord)

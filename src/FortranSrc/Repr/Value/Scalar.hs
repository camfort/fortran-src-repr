{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-} -- used for better inference (maybe)

module FortranSrc.Repr.Value.Scalar where

import FortranSrc.Repr.Value.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Complex
import FortranSrc.Repr.Type.Scalar
import FortranSrc.Repr.Type.Scalar.Int
import FortranSrc.Repr.Value.Common
import Data.Kind

data FV (pr :: PrimRepr)
  = FVInt     (SomeFInt     pr)
  | FVReal    (SomeFReal    pr)
  | FVComplex (SomeFComplex pr)
    deriving stock (Show)

type FLogical :: PrimRepr -> FTInt -> Type
type family FLogical pr k where
    FLogical 'Machine   k = FIntMRep k
    FLogical 'Idealized _ = Bool
data SomeFLogical pr where
    SomeFLogical :: FLogical pr k -> SomeFLogical pr

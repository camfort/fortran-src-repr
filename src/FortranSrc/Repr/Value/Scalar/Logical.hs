module FortranSrc.Repr.Value.Scalar.Logical where

import FortranSrc.Repr.Value.Common
import FortranSrc.Repr.Type.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Int
import Data.Singletons

data SomeFLogical pr =
    forall (k :: FTInt). SomeFLogical (Sing k) (FLogical pr k)
deriving stock instance Show (SomeFLogical pr)

data FLogical (pr :: PrimRepr) (k :: FTInt) where
    FLogicalM :: FInt 'Machine k -> FLogical 'Machine k
    FLogicalI :: Bool -> FLogical 'Idealized k
deriving stock instance Show (FLogical pr k)

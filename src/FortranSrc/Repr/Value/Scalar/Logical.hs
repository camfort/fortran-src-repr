module FortranSrc.Repr.Value.Scalar.Logical where

import FortranSrc.Repr.Value.Common
import FortranSrc.Repr.Type.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Int
import Data.Singletons

data SomeFLogical pr =
    forall (k :: FTInt). SomeFLogical (Sing k) (FLogical pr k)
deriving stock instance Show (SomeFLogical pr)
instance Eq (SomeFLogical pr) where
    (SomeFLogical _ (FLogicalI b1)) == (SomeFLogical _ (FLogicalI b2)) = b1 == b2
    (SomeFLogical k1 (FLogicalM a1)) == (SomeFLogical k2 (FLogicalM a2)) =
        someFIntMBinOp (==) (SomeFInt k1 a1) (SomeFInt k2 a2)
instance Ord (SomeFLogical pr) where
    compare (SomeFLogical _ (FLogicalI b1)) (SomeFLogical _ (FLogicalI b2)) = compare b1 b2
    compare (SomeFLogical k1 (FLogicalM a1)) (SomeFLogical k2 (FLogicalM a2)) =
        someFIntMBinOp compare (SomeFInt k1 a1) (SomeFInt k2 a2)

data FLogical (pr :: PrimRepr) (k :: FTInt) where
    FLogicalM :: FInt 'Machine k -> FLogical 'Machine k
    FLogicalI :: Bool -> FLogical 'Idealized k
deriving stock instance Show (FLogical pr k)
deriving stock instance Eq   (FLogical pr k)
deriving stock instance Ord  (FLogical pr k)

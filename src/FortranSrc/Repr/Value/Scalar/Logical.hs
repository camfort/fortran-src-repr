module FortranSrc.Repr.Value.Scalar.Logical where

import FortranSrc.Repr.Value.Common
import FortranSrc.Repr.Type.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Int
import Data.Singletons
import FortranSrc.Repr.Type.Scalar.Common
import Data.Type.Equality

data SomeFLogical pr =
    forall (k :: FTInt). SomeFLogical (Sing k) (FLogical pr k)
deriving stock instance Show (SomeFLogical pr)
instance Eq (SomeFLogical pr) where
    (SomeFLogical _ (FLogicalI b1)) == (SomeFLogical _ (FLogicalI b2)) = b1 == b2
    (SomeFLogical k1 (FLogicalM (FIntM i1))) == (SomeFLogical k2 (FLogicalM (FIntM i2))) =
        case singCompare k1 k2 of
          SingEq Refl -> i1 == i2
          SingLt      -> fromIntegral i1 == i2
          SingGt      -> i1 == fromIntegral i2

data FLogical (pr :: PrimRepr) (k :: FTInt) where
    FLogicalM :: FInt 'Machine k -> FLogical 'Machine k
    FLogicalI :: Bool -> FLogical 'Idealized k
deriving stock instance Show (FLogical pr k)
deriving stock instance Eq   (FLogical pr k)

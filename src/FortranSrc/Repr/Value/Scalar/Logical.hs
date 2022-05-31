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
    (SomeFLogical k1 (FLogicalM i1)) == (SomeFLogical k2 (FLogicalM i2)) =
        someFIntMBinOp (==) (SomeFInt k1 (FIntM i1)) (SomeFInt k2 (FIntM i2))
instance Ord (SomeFLogical pr) where
    compare (SomeFLogical _ (FLogicalI b1)) (SomeFLogical _ (FLogicalI b2)) = compare b1 b2
    compare (SomeFLogical k1 (FLogicalM i1)) (SomeFLogical k2 (FLogicalM i2)) =
        someFIntMBinOp compare (SomeFInt k1 (FIntM i1)) (SomeFInt k2 (FIntM i2))

data FLogical (pr :: PrimRepr) (k :: FTInt) where
    FLogicalM :: (rep ~ FIntMRep k, Integral rep, Show rep) => rep -> FLogical 'Machine k
    FLogicalI :: Bool -> FLogical 'Idealized k
deriving stock instance Show (FLogical pr k)
deriving stock instance Eq   (FLogical pr k)
deriving stock instance Ord  (FLogical pr k)

someFLogicalNot :: SomeFLogical pr -> SomeFLogical pr
someFLogicalNot (SomeFLogical k b) =
    case b of
      FLogicalM i  ->
        case i of
          1 -> SomeFLogical k $ FLogicalM 0
          _ -> SomeFLogical k $ FLogicalM 1
      FLogicalI b' -> SomeFLogical k $ FLogicalI $ not b'

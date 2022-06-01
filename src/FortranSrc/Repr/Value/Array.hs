{-# LANGUAGE AllowAmbiguousTypes #-}

-- unwrapping somes gets you kind and length at the same time - I figure because
-- kind is just a convenience.

module FortranSrc.Repr.Value.Array where

import FortranSrc.Repr.Type.Array
import FortranSrc.Repr.Type.Scalar
import FortranSrc.Repr.Type.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Int
import FortranSrc.Repr.Type.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Complex
import FortranSrc.Repr.Value.Scalar.String
import FortranSrc.Repr.Util ( natVal'' )

import Data.Vector.Sized qualified as V
import Data.Vector.Sized ( Vector )
import GHC.TypeNats
import Data.Kind
import Data.Singletons

type Size :: [Natural] -> Natural
type family Size dims where
    Size (dim ': dims) = dim + Size dims
    Size '[]           = 0

-- can conveniently define kinded array types like so
data FVA (ft :: k -> Type) (fk :: k) (dims :: [Natural])
  = FVA { unFVA :: Vector (Size dims) (ft fk) }
deriving stock instance Show (ft fk) => Show (FVA ft fk dims)

-- makes rank 1 array
mkFVA1 :: forall l ft fk. Vector l (ft fk) -> FVA ft fk '[l]
mkFVA1 = FVA

-- reifies type info
fvaShape :: forall dims ft fk. KnownNats dims => FVA ft fk dims -> Shape
fvaShape _ = Shape $ natVals @dims

mkSomeFVA :: (forall l. KnownNat l => Vector l a -> r) -> [a] -> r
mkSomeFVA f as = V.withSizedList as f

-- | Reify a list of type-level 'Natural's.
class KnownNats (ns :: [Natural]) where natVals :: [Natural]
instance (KnownNat n, KnownNats ns) => KnownNats (n ': ns) where
    natVals = natVal'' @n : natVals @ns
instance KnownNats '[] where natVals = []

data SomeFVA k ft =
    forall (fk :: k) (dims :: [Natural]). (KnownNats dims, SingKind k, SingI fk)
        => SomeFVA { unSomeFVA :: FVA ft fk dims }
deriving stock instance Show (SomeFVA FTInt   FIntM)
deriving stock instance Show (SomeFVA FTReal  FReal)
deriving stock instance Show (SomeFVA FTReal  FComplex)
deriving stock instance Show (SomeFVA Natural FString)

someFVAKind :: SomeFVA k ft -> Demote k
someFVAKind (SomeFVA (_ :: FVA ft fk dims)) = demote @fk

someFVAShape :: SomeFVA k ft -> Shape
someFVAShape (SomeFVA a) = fvaShape a

-- makes rank 1 array
mkSomeFVA1
    :: forall k ft (fk :: k). (SingKind k, SingI fk)
    => [ft fk] -> SomeFVA k ft
mkSomeFVA1 = mkSomeFVA $ SomeFVA . mkFVA1

data FVAM
  = FVAMInt     (SomeFVA FTInt   FIntM)
  | FVAMReal    (SomeFVA FTReal  FReal)
  | FVAMComplex (SomeFVA FTReal  FComplex)
  | FVAMLogical (SomeFVA FTInt   FIntM)
  | FVAMString  (SomeFVA Natural FString)
deriving stock instance Show FVAM

fvaType :: FVAM -> FTA
fvaType = \case
  FVAMInt     a -> go FTInt     a
  FVAMReal    a -> go FTReal    a
  FVAMComplex a -> go FTComplex a
  FVAMLogical a -> go FTLogical a
  FVAMString  a -> go FTString  a
  where
    go :: (Demote k -> FTS) -> SomeFVA k ft -> FTA
    go f a = FTA (f (someFVAKind a)) (someFVAShape a)
{-
  FVAMReal    a -> 
  FVAMComplex a -> 
  FVAMLogical a -> 
  FVAMString  a -> 
-}

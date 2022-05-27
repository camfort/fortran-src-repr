{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-} -- used for better inference (maybe)

module FortranSrc.Repr.Value.Scalar where

import FortranSrc.Repr.Type.Scalar
import FortranSrc.Repr.Value.Common
import Data.Kind
import Data.Int
import Data.Singletons
import Data.Ord.Singletons
import Data.Type.Equality

{-
data SomeFInt pr where
    SomeFIntM :: forall (k :: FTInt). Show (FIntMRep k) => FIntM k -> SomeFInt 'Machine
    SomeFIntI :: forall (k :: FTInt). FIntI k -> SomeFInt 'Idealized
deriving stock instance Show (SomeFInt pr)
-}

-- TODO Sing or SFTInt? both seem to work (...?)
data SomeFInt pr =
    forall (k :: FTInt). (Show (FIntMRep k), Show (FIntRep pr k), Integral (FIntMRep k)) => SomeFInt (Sing k) (FIntRep pr k)
deriving stock instance Show (SomeFInt pr)

newtype FIntM (k :: FTInt) = FIntM (FIntMRep k)
deriving stock instance Show (FIntMRep k) => Show (FIntM k)

type FIntRep :: PrimRepr -> FTInt -> Type
-- can't write injectivity annotation because RHS is type family
type family FIntRep pr k where
    FIntRep 'Machine   k = FIntMRep k
    FIntRep 'Idealized _ = Integer

type FIntMRep :: FTInt -> Type
type family FIntMRep k = r | r -> k where
    FIntMRep 'FTInt1 = Int8
    FIntMRep 'FTInt2 = Int16
    FIntMRep 'FTInt4 = Int32
    FIntMRep 'FTInt8 = Int64

newtype FIntI (k :: FTInt) = FIntI Integer
    deriving stock (Show)

fIntCheckBounds
    :: forall k rep. (rep ~ FIntMRep k, Bounded rep, Integral rep)
    => FIntI k -> Maybe String
fIntCheckBounds (FIntI i) =
    if   i > fromIntegral (maxBound @rep)
    then Just "TODO too large"
    else if   i < fromIntegral (minBound @rep)
         then Just "TODO too small"
         else Nothing

data FV (pr :: PrimRepr)
  = FVInt (SomeFInt pr)

class FIntAdd (pr :: PrimRepr) where
    fIntAdd :: SomeFInt pr -> SomeFInt pr -> SomeFInt pr

instance FIntAdd 'Machine where
    fIntAdd (SomeFInt k1 i1) (SomeFInt k2 i2) = case ftIntKindCmp k1 k2 of
      SingEq Refl -> SomeFInt k1 (i1+i2)
      SingLt      -> SomeFInt k2 (fromIntegral i1+i2)
      SingGt      -> SomeFInt k1 (i1+fromIntegral i2)

instance FIntAdd 'Idealized where
    fIntAdd (SomeFInt k1 i1) (SomeFInt k2 i2) = case ftIntKindCmp k1 k2 of
      SingEq Refl -> SomeFInt k1 (i1+i2)
      SingLt      -> SomeFInt k2 (i1+i2)
      SingGt      -> SomeFInt k1 (i1+i2)

data SomeFReal pr where
    SomeFReal :: FReal pr k -> SomeFReal pr
type FReal (pr :: PrimRepr) (k :: FTReal) = Double

data SomeFComplex pr where
    SomeFComplex :: FReal pr k -> FReal pr k -> SomeFComplex pr

type FLogical :: PrimRepr -> FTInt -> Type
type family FLogical pr k where
    FLogical 'Machine   k = FIntMRep k
    FLogical 'Idealized _ = Bool
data SomeFLogical pr where
    SomeFLogical :: FLogical pr k -> SomeFLogical pr

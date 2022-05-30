{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-} -- just for better inference (maybe)

module FortranSrc.Repr.Value.Scalar.Int where

import FortranSrc.Repr.Type.Scalar.Common
import FortranSrc.Repr.Type.Scalar.Int
import FortranSrc.Repr.Value.Common
import Data.Kind
import Data.Int
import Data.Singletons
import Data.Type.Equality

-- TODO Sing or SFTInt? both seem to work (...?)
data SomeFInt pr =
    forall (k :: FTInt). SomeFInt (Sing k) (FInt pr k)
deriving stock instance Show (SomeFInt pr)
instance Eq (SomeFInt pr) where
    (SomeFInt _ (FIntI i1)) == (SomeFInt _ (FIntI i2)) = i1 == i2
    a1@(SomeFInt _ FIntM{}) == a2@(SomeFInt _ FIntM{}) = someFIntMBinOp (==) a1 a2
instance Ord (SomeFInt pr) where
    compare (SomeFInt _ (FIntI i1)) (SomeFInt _ (FIntI i2)) = compare i1 i2
    compare a1@(SomeFInt _ FIntM{}) a2@(SomeFInt _ FIntM{}) = someFIntMBinOp compare a1 a2

someFIntMBinOp
    :: (forall a. Integral a => a -> a -> b)
    -> SomeFInt 'Machine -> SomeFInt 'Machine -> b
someFIntMBinOp f (SomeFInt k1 (FIntM i1)) (SomeFInt k2 (FIntM i2)) =
    case singCompare k1 k2 of
      SingEq Refl ->              i1 `f`              i2
      SingLt      -> fromIntegral i1 `f`              i2
      SingGt      ->              i1 `f` fromIntegral i2

-- | Recover some @INTEGER(x)@'s kind (the @x@).
someFIntKind :: SomeFInt pr -> FTInt
someFIntKind (SomeFInt s _) = fromSing s

data FInt (pr :: PrimRepr) (k :: FTInt) where
    FIntM :: (rep ~ FIntMRep k, Integral rep, Show rep) => rep -> FInt 'Machine k
    FIntI :: Integer -> FInt 'Idealized k
deriving stock instance Show (FInt pr k)
deriving stock instance Eq   (FInt pr k)
deriving stock instance Ord  (FInt pr k)

type FIntMRep :: FTInt -> Type
type family FIntMRep k = r | r -> k where
    FIntMRep 'FTInt1 = Int8
    FIntMRep 'FTInt2 = Int16
    FIntMRep 'FTInt4 = Int32
    FIntMRep 'FTInt8 = Int64

fIntCheckBounds
    :: forall k rep. (rep ~ FIntMRep k, Bounded rep, Integral rep)
    => Integer -> Maybe String
fIntCheckBounds i =
    if   i > fromIntegral (maxBound @rep)
    then Just "TODO too large"
    else if   i < fromIntegral (minBound @rep)
         then Just "TODO too small"
         else Nothing

fIntAdd :: SomeFInt pr -> SomeFInt pr -> SomeFInt pr
fIntAdd (SomeFInt k1 (FIntI i1)) (SomeFInt k2 (FIntI i2)) =
    case singCompare k1 k2 of
      SingEq Refl -> SomeFInt k1 (FIntI (i1+i2))
      SingLt      -> SomeFInt k2 (FIntI (i1+i2))
      SingGt      -> SomeFInt k1 (FIntI (i1+i2))
fIntAdd (SomeFInt k1 (FIntM i1)) (SomeFInt k2 (FIntM i2)) =
    case singCompare k1 k2 of
      SingEq Refl -> SomeFInt k1 (FIntM (i1+i2))
      SingLt      -> SomeFInt k2 (FIntM (fromIntegral i1+i2))
      SingGt      -> SomeFInt k1 (FIntM (i1+fromIntegral i2))

fIntUse :: (forall a. Integral a => a -> b) -> SomeFInt pr -> b
fIntUse f (SomeFInt _ x) = case x of FIntI i -> f i
                                     FIntM i -> f i

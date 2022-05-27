{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module FortranSrc.Repr.Type.Scalar where

import GHC.Generics ( Generic )
import Data.Data ( Data )

import Data.Kind
import GHC.TypeNats
import GHC.Exts

import Data.Singletons.TH
-- required for deriving instances (seems like bug)
import Prelude.Singletons
import Data.Ord.Singletons
import Unsafe.Coerce

type FKind = Natural

$(singletons [d|
    -- TODO INTEGER(16) (gfortran supports)
    data FTInt
      = FTInt1
      | FTInt2
      | FTInt4
      | FTInt8
        deriving stock (Eq, Ord, Show)
    |])
deriving stock instance Generic FTInt
deriving stock instance Data    FTInt
deriving stock instance Enum FTInt

ftIntKind :: FTInt -> Natural
ftIntKind (FromSing i) = case i of
 SFTInt1 -> reifyKinded i
 SFTInt2 -> reifyKinded i
 SFTInt4 -> reifyKinded i
 SFTInt8 -> reifyKinded i

data SingCmp (l :: k) (r :: k)
  = SingEq (l :~: r)
  | SingLt
  | SingGt

ftIntKindCmp
    :: forall (k1 :: FTInt) (k2 :: FTInt)
    .  SFTInt k1 -> SFTInt k2 -> SingCmp k1 k2
ftIntKindCmp k1 k2 =
    case k1 `sCompare` k2 of
      SEQ -> SingEq (unsafeCoerce Refl) -- Lol this is probably fine
      SLT -> SingLt
      SGT -> SingGt
{-
    case (k1, k2) of
  (SFTInt1, SFTInt1) -> SingEq Refl
  (SFTInt2, SFTInt2) -> SingEq Refl
  (SFTInt4, SFTInt4) -> SingEq Refl
  (SFTInt8, SFTInt8) -> SingEq Refl
-}

-- | Reify a kind tag.
reifyKinded
    :: forall k (a :: k) n. (n ~ FKindOf a, KnownNat n)
    => Sing a -> FKind
reifyKinded _ = natVal'' @n

-- | Fortran types which use simple integer kinds.
class FKinded (a :: Type) where
    type FKindOf (x :: a) :: Natural

    -- | This we get via the type family, but require singletons.
    printFKind :: a -> FKind

    -- | This we *should* get via the type family, but again require singletons.
    parseFKind :: FKind -> Maybe a

instance FKinded FTInt where
    type FKindOf 'FTInt1 = 1
    type FKindOf 'FTInt2 = 2
    type FKindOf 'FTInt4 = 4
    type FKindOf 'FTInt8 = 4
    parseFKind = \case 1 -> Just FTInt1
                       2 -> Just FTInt1
                       4 -> Just FTInt1
                       8 -> Just FTInt1
                       _ -> Nothing
    printFKind = \case FTInt1 -> 1
                       FTInt2 -> 2
                       FTInt4 -> 4
                       FTInt8 -> 8

data FTReal
  = FTReal4
  | FTReal8
    deriving stock    (Generic, Data, Show, Eq, Ord, Enum)

instance FKinded FTReal where
    type FKindOf 'FTReal4 = 4
    type FKindOf 'FTReal8 = 8
    parseFKind = \case 4 -> Just FTReal4
                       8 -> Just FTReal8
                       _ -> Nothing
    printFKind = \case FTReal4 -> 4
                       FTReal8 -> 8

data FTComplex
  = FTComplex8
  | FTComplex16
    deriving stock    (Generic, Data, Show, Eq, Ord, Enum)

instance FKinded FTComplex where
    type FKindOf 'FTComplex8  = 8
    type FKindOf 'FTComplex16 = 16
    parseFKind = \case 8  -> Just FTComplex8
                       16 -> Just FTComplex16
                       _  -> Nothing
    printFKind = \case FTComplex8  -> 8
                       FTComplex16 -> 16

-- | The length of a CHARACTER value.
--
-- IanH provides a great reference on StackOverflow:
-- https://stackoverflow.com/a/25051522/2246637
data CharLen
  = CharLen Integer
  -- ^ @CHARACTER(LEN=x)@ (where @x@ is a constant integer expression). Value
  --   has the given static length.

  | CharLenAssumed
  -- ^ @CHARACTER(LEN=*)@. F90. Value has assumed length. For a dummy argument,
  --   the length is assumed from the actual argument. For a PARAMETER named
  --   constant, the length is assumed from the length of the initializing
  --   expression.

  | CharLenDeferred
  -- ^ @CHARACTER(LEN=:)@. F2003. Value has deferred length. Must have the
  --   ALLOCATABLE or POINTER attribute.

    deriving stock    (Generic, Data, Show, Eq, Ord)

prettyCharLen :: CharLen -> String
prettyCharLen = \case
  CharLen l       -> "LEN="<>show l
  CharLenAssumed  -> "LEN=*"
  CharLenDeferred -> "LEN=:"

data FTS
  = FTInt FTInt
  | FTReal FTReal
  | FTComplex FTComplex
  | FTLogical FTInt
  | FTChar CharLen
  | FTCustom String     -- ^ F77 structure, F90 DDT (non-intrinsic scalar)
    deriving stock    (Generic, Data, Show, Eq, Ord)

prettyFTS :: FTS -> String
prettyFTS = \case
  FTInt     k -> prettyKinded k "INTEGER"
  FTReal    k -> prettyKinded k "REAL"
  FTComplex k -> prettyKinded k "COMPLEX"
  FTLogical k -> prettyKinded k "LOGICAL"
  FTChar    k -> "CHARACTER("<>prettyCharLen k<>")"
  FTCustom  t -> "TYPE("<>t<>")"

prettyKinded :: FKinded a => a -> String -> String
prettyKinded k name = name<>"("<>show (printFKind k)<>")"

--------------------------------------------------------------------------------

natVal'' :: forall a. KnownNat a => Natural
natVal'' = natVal' (proxy# :: Proxy# a)

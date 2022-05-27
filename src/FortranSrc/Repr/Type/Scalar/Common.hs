module FortranSrc.Repr.Type.Scalar.Common where

import FortranSrc.Repr.Util

import Data.Kind
import GHC.TypeNats

import Data.Type.Equality
import Data.Ord.Singletons
import Unsafe.Coerce

type FKind = Natural

data SingCmp (l :: k) (r :: k)
  = SingEq (l :~: r)
  | SingLt
  | SingGt

singCompare
    :: forall k (a :: k) (b :: k). SOrd k
    => Sing a -> Sing b -> SingCmp a b
singCompare a b =
    case a `sCompare` b of
      SEQ -> SingEq (unsafeCoerce Refl) -- Lol this is probably fine
      SLT -> SingLt
      SGT -> SingGt

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

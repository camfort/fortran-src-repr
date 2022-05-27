module FortranSrc.Repr.Value.Scalar.String where

import FortranSrc.Repr.Value.Common
import FortranSrc.Repr.Type.Scalar.String
import GHC.TypeNats
import Data.Text ( Text )
import Data.Text qualified as Text
import FortranSrc.Repr.Util ( natVal'' )
import Data.Singletons
import GHC.TypeLits.Singletons ( SNat(..) )
import Unsafe.Coerce

data SomeFString pr = forall (l :: CharLen). SomeFString (Sing l) (FString pr l)
deriving stock instance Show (SomeFString pr)
instance Eq (SomeFString pr) where
    (SomeFString _ (FString s1)) == (SomeFString _ (FString s2)) = s1 == s2

-- TODO unsafe constructor do not use >:(
data FString (pr :: PrimRepr) (l :: CharLen) where
    FString :: KnownNat n => Text -> FString pr ('CharLen n)
deriving stock instance Show (FString pr l)
deriving stock instance Eq   (FString pr l)

fString :: forall n pr. KnownNat n => Text -> Maybe (FString pr ('CharLen n))
fString s =
    if   Text.length s == fromIntegral (natVal'' @n)
    then Just $ FString s
    else Nothing

someFString :: Text -> SomeFString pr
someFString s =
    case someNatVal (fromIntegral (Text.length s)) of
      SomeNat (_ :: Proxy n) -> SomeFString (SCharLen @n SNat) $ FString s

-- TODO dunno how to do this without unsafeCoerce because of the type-level nat
-- addition >:(
concatFString
    :: forall pr l1 l2. (KnownNat l1, KnownNat l2)
    => FString pr ('CharLen l1)
    -> FString pr ('CharLen l2)
    -> FString pr ('CharLen (l1 + l2))
concatFString (FString s1) (FString s2) = unsafeCoerce (FString @l1 $ s1 <> s2)

concatSomeFString
    :: forall pr. SomeFString pr -> SomeFString pr -> SomeFString pr
concatSomeFString (SomeFString (SCharLen SNat) s1) (SomeFString (SCharLen SNat) s2) =
    case concatFString s1 s2 of
      s3@(FString _) -> SomeFString (SCharLen SNat) s3
concatSomeFString _ _ = error "undefined (but shouldn't be possible for now)"

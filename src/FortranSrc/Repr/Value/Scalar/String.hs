module FortranSrc.Repr.Value.Scalar.String where

import FortranSrc.Repr.Value.Common
import FortranSrc.Repr.Type.Scalar.String
import GHC.TypeNats
import Data.Text ( Text )
import Data.Text qualified as Text
import FortranSrc.Repr.Util ( natVal'' )
import Data.Singletons
import GHC.TypeLits.Singletons ( SNat(..) )

data SomeFString pr = forall (l :: CharLen). SomeFString (Sing l) (FString pr l)
deriving stock instance Show (SomeFString pr)

-- TODO unsafe constructor do not use >:(
data FString (pr :: PrimRepr) (l :: CharLen) where
    FString :: KnownNat n => Text -> FString pr ('CharLen n)
deriving stock instance Show (FString pr l)

fString :: forall n pr. KnownNat n => Text -> Maybe (FString pr ('CharLen n))
fString s =
    if   Text.length s == fromIntegral (natVal'' @n)
    then Just $ FString s
    else Nothing

someFString :: Text -> SomeFString pr
someFString s =
    case someNatVal (fromIntegral (Text.length s)) of
      SomeNat (_ :: Proxy n) -> SomeFString (SCharLen @n SNat) $ FString s

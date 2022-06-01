module FortranSrc.Repr.Value.Scalar.Common where

import Data.Singletons

data SomeFKinded k ft = forall (fk :: k). (SingKind k, SingI fk) => SomeFKinded (ft fk)

-- | Recover some @TYPE(x)@'s kind (the @x@).
someFKindedKind :: SomeFKinded k ft -> Demote k
someFKindedKind (SomeFKinded (_ :: ft fk)) = demote @fk

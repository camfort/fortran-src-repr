module FortranSrc.Repr.Type.Scalar.Complex where

import FortranSrc.Repr.Type.Scalar.Common
import FortranSrc.Repr.Type.Scalar.Real

import GHC.Generics ( Generic )
import Data.Data ( Data )

newtype FTComplex = FTComplex { unFTComplex :: FTReal }
    deriving stock (Generic, Data, Show, Eq, Ord)

instance FKinded FTComplex where
    type FKindOf ('FTComplex 'FTReal4) = 8
    type FKindOf ('FTComplex 'FTReal8) = 16
    parseFKind = \case 8  -> Just $ FTComplex FTReal4
                       16 -> Just $ FTComplex FTReal8
                       _ -> Nothing
    printFKind = \case
      FTComplex FTReal4 -> 8
      FTComplex FTReal8 -> 16

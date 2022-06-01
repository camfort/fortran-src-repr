module FortranSrc.Repr.Type.Scalar.Complex where

import FortranSrc.Repr.Type.Scalar.Common
import FortranSrc.Repr.Type.Scalar.Real

import GHC.Generics ( Generic )
import Data.Data ( Data )

newtype FTComplexWrapper = FTComplexWrapper { unFTComplexWrapper :: FTReal }
    deriving stock (Generic, Data, Show, Eq, Ord)

instance FKinded FTComplexWrapper where
    type FKindOf ('FTComplexWrapper 'FTReal4) = 8
    type FKindOf ('FTComplexWrapper 'FTReal8) = 16
    parseFKind = \case 8  -> Just $ FTComplexWrapper FTReal4
                       16 -> Just $ FTComplexWrapper FTReal8
                       _ -> Nothing
    printFKind = \case
      FTComplexWrapper FTReal4 -> 8
      FTComplexWrapper FTReal8 -> 16

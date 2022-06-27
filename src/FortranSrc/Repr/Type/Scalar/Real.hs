{-# LANGUAGE TemplateHaskell #-}

module FortranSrc.Repr.Type.Scalar.Real where

import FortranSrc.Repr.Type.Scalar.Common

import GHC.Generics ( Generic )
import Data.Data ( Data )

import Data.Singletons.TH
-- required for deriving instances (seems like bug)
import Prelude.Singletons
import Data.Ord.Singletons

$(singletons [d|
    data FTReal
      = FTReal4
      | FTReal8
        deriving stock (Eq, Ord, Show)
    |])
deriving stock instance Generic FTReal
deriving stock instance Data    FTReal
deriving stock instance Enum    FTReal

instance FKinded FTReal where
    type FKindOf 'FTReal4 = 4
    type FKindOf 'FTReal8 = 8
    type FKindDefault = 'FTReal4
    parseFKind = \case 4 -> Just FTReal4
                       8 -> Just FTReal8
                       _ -> Nothing
    printFKind (FromSing x) = case x of
      SFTReal4 -> reifyKinded x
      SFTReal8 -> reifyKinded x

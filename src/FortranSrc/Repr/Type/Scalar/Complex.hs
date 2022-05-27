{-# LANGUAGE TemplateHaskell #-}

module FortranSrc.Repr.Type.Scalar.Complex where

import FortranSrc.Repr.Type.Scalar.Common

import GHC.Generics ( Generic )
import Data.Data ( Data )

import Data.Singletons.TH
-- required for deriving instances (seems like bug)
import Prelude.Singletons
import Data.Ord.Singletons

$(singletons [d|
    data FTComplex
      = FTComplex8
      | FTComplex16
        deriving stock (Eq, Ord, Show)
    |])
deriving stock instance Generic FTComplex
deriving stock instance Data    FTComplex
deriving stock instance Enum    FTComplex

instance FKinded FTComplex where
    type FKindOf 'FTComplex8  = 8
    type FKindOf 'FTComplex16 = 16
    parseFKind = \case 8  -> Just FTComplex8
                       16 -> Just FTComplex16
                       _  -> Nothing
    printFKind (FromSing x) = case x of
      SFTComplex8  -> reifyKinded x
      SFTComplex16 -> reifyKinded x

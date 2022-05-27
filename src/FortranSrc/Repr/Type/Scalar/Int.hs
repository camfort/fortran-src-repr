{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module FortranSrc.Repr.Type.Scalar.Int where

import FortranSrc.Repr.Type.Scalar.Common
import FortranSrc.Repr.Util

import GHC.Generics ( Generic )
import Data.Data ( Data )

import Data.Kind
import GHC.TypeNats

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
deriving stock instance Enum    FTInt

instance FKinded FTInt where
    type FKindOf 'FTInt1 = 1
    type FKindOf 'FTInt2 = 2
    type FKindOf 'FTInt4 = 4
    type FKindOf 'FTInt8 = 8
    parseFKind = \case 1 -> Just FTInt1
                       2 -> Just FTInt2
                       4 -> Just FTInt4
                       8 -> Just FTInt8
                       _ -> Nothing
    printFKind (FromSing x) = case x of
      SFTInt1 -> reifyKinded x
      SFTInt2 -> reifyKinded x
      SFTInt4 -> reifyKinded x
      SFTInt8 -> reifyKinded x

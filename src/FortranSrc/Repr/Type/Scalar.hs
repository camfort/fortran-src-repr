{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module FortranSrc.Repr.Type.Scalar where

import FortranSrc.Repr.Type.Scalar.Common
import FortranSrc.Repr.Type.Scalar.Int
import FortranSrc.Repr.Type.Scalar.Real
import FortranSrc.Repr.Type.Scalar.Complex

import GHC.Generics ( Generic )
import Data.Data ( Data )

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

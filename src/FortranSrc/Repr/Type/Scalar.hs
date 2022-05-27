module FortranSrc.Repr.Type.Scalar where

import FortranSrc.Repr.Type.Scalar.Common
import FortranSrc.Repr.Type.Scalar.Int
import FortranSrc.Repr.Type.Scalar.Real
import FortranSrc.Repr.Type.Scalar.Complex
import FortranSrc.Repr.Type.Scalar.String

import GHC.Generics ( Generic )
import Data.Data ( Data )

data FTS
  = FTInt FTInt
  | FTReal FTReal
  | FTComplex FTComplex
  | FTLogical FTInt
  | FTString CharLen
  | FTCustom String     -- ^ F77 structure, F90 DDT (non-intrinsic scalar)
    deriving stock (Generic, Data, Show, Eq, Ord)

prettyFTS :: FTS -> String
prettyFTS = \case
  FTInt     k -> prettyKinded k "INTEGER"
  FTReal    k -> prettyKinded k "REAL"
  FTComplex k -> prettyKinded k "COMPLEX"
  FTLogical k -> prettyKinded k "LOGICAL"
  FTString  l -> "CHARACTER("<>prettyCharLen l<>")"
  FTCustom  t -> "TYPE("<>t<>")"

prettyKinded :: FKinded a => a -> String -> String
prettyKinded k name = name<>"("<>show (printFKind k)<>")"

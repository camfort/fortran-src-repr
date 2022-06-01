module FortranSrc.Repr.Type where

import FortranSrc.Repr.Type.Scalar
import FortranSrc.Repr.Type.Array

data FT = FTA FTA | FTS FTS
    deriving stock Show

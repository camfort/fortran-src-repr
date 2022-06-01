module FortranSrc.Repr.Value where

import FortranSrc.Repr.Value.Scalar
import FortranSrc.Repr.Value.Array

data FVM = FVAM FVAM | FVSM FVSM
    deriving stock Show

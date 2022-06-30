module FortranSrc.Repr.Eval.Common where

import qualified Language.Fortran.AST as F
import FortranSrc.Repr.Value

class Monad m => MonadEval m where
    type EvalTo m
    lookupFVar :: F.Name -> m (Maybe (EvalTo m))

    -- | Arbitrarily record some user-facing information concerning evaluation.
    --
    -- For example, potentially useful when making defaulting decisions.
    warn :: String -> m ()

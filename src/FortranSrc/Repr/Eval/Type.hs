module FortranSrc.Repr.Eval.Type where

import qualified Language.Fortran.AST as F
import FortranSrc.Repr.Type
import FortranSrc.Repr.Eval.Common

fromExpression
    :: forall m a. (MonadEval m, EvalTo m ~ FType)
    => F.Expression a -> m (Either String FType)
fromExpression = \case
  F.ExpValue _ _ (F.ValVariable name) ->
    lookupFVar name >>= \case
      Nothing  -> return $ Left "no such variable found TODO"
      Just val -> return $ Right val

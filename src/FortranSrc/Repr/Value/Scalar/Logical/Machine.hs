module FortranSrc.Repr.Value.Scalar.Logical.Machine where

import FortranSrc.Repr.Type.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Int.Machine

someFLogicalNot :: SomeFInt -> SomeFInt
someFLogicalNot = someFIntUOpWrap $ \bi -> if bi == 1 then 0 else 1

-- | Wrap a boolean into a Fortran "machine" boolean (represented by a
--   integer of default kind).
--   TODO
--fLogical :: Bool -> FInt (FKindDefault FTInt)
toFLogical :: Bool -> FInt 'FTInt4
toFLogical = \case True -> FInt4 1; False -> FInt4 0

-- | Retrieve the boolean value stored by a @LOGICAL(x)@.
--
-- TODO confirm correctness
fromSomeFLogical :: SomeFInt -> Bool
fromSomeFLogical = someFIntUOp $ \i -> if i == 1 then True else False

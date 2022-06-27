module FortranSrc.Repr.Value.Scalar.Logical.Machine where

import FortranSrc.Repr.Type.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Int.Machine

someFLogicalNot :: SomeFInt -> SomeFInt
someFLogicalNot = someFIntUOpWrap $ \bi -> if bi == 1 then 0 else 1

-- | Wrap a boolean into a Fortran "machine" boolean (represented by a
--   integer of default kind).
--   TODO
--fLogical :: Bool -> FInt (FKindDefault FTInt)
fLogical :: Bool -> FInt 'FTInt4
fLogical = \case True -> FInt4 1; False -> FInt4 0

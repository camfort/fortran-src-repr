module FortranSrc.Repr
  (
  -- * Assorted notes
  -- ** Design aims
  -- $design-aims

  -- ** Kind semantics
  -- $kind-semantics

  -- ** Naming conventions
  -- $naming-conventions

  -- * Re-exports
  -- ** Type representation
    module FortranSrc.Repr.Type
  , module FortranSrc.Repr.Type.Array
  , module FortranSrc.Repr.Type.Scalar
  , module FortranSrc.Repr.Type.Scalar.Common
  , module FortranSrc.Repr.Type.Scalar.Int
  , module FortranSrc.Repr.Type.Scalar.Real
  , module FortranSrc.Repr.Type.Scalar.Complex
  , module FortranSrc.Repr.Type.Scalar.String

  -- ** Value representation
  , module FortranSrc.Repr.Value
  , module FortranSrc.Repr.Value.Array
  , module FortranSrc.Repr.Value.Scalar
  , module FortranSrc.Repr.Value.Scalar.Common
  , module FortranSrc.Repr.Value.Scalar.Int
  , module FortranSrc.Repr.Value.Scalar.Real
  , module FortranSrc.Repr.Value.Scalar.Complex
  , module FortranSrc.Repr.Value.Scalar.Logical
  , module FortranSrc.Repr.Value.Scalar.String
  ) where

import FortranSrc.Repr.Type
import FortranSrc.Repr.Type.Array
import FortranSrc.Repr.Type.Scalar
import FortranSrc.Repr.Type.Scalar.Common
import FortranSrc.Repr.Type.Scalar.Int
import FortranSrc.Repr.Type.Scalar.Real
import FortranSrc.Repr.Type.Scalar.Complex
import FortranSrc.Repr.Type.Scalar.String

import FortranSrc.Repr.Value
import FortranSrc.Repr.Value.Array
import FortranSrc.Repr.Value.Scalar
import FortranSrc.Repr.Value.Scalar.Common
import FortranSrc.Repr.Value.Scalar.Int
import FortranSrc.Repr.Value.Scalar.Real
import FortranSrc.Repr.Value.Scalar.Complex
import FortranSrc.Repr.Value.Scalar.Logical
import FortranSrc.Repr.Value.Scalar.String

{- $design-aims

The aims for this representation are _correctness_ and _efficiency_. All values
store enough information on the type level to recover their precise Fortran
type via inspection.

Where possible, this representation matches common exceptional behaviours in
Fortran expression evaluation - specifically using gfortran as a basis. For
example:

  * Integers overflow predictably.
  * Reals should have approximately matching behaviour, since both gfortran and
    Haskell use IEEE floats.
-}

{- $kind-semantics

Kinds in Fortran are natural number "tags" associated with certain intrinsic
types. They enable Fortran implementations to group similar types of value
together under the same Fortran type. That is, though an @INTEGER(4)@ and an
@INTEGER(8)@ are both integers, most Fortran compilers will use different
representations for the values. We match this in Haskell by defining a sum type
for a given Fortran type, and making a constructor for each valid kind.

Fortran standards do not specify full semantics for kinds, only things like
interactions and precision requirements. However, average modern Fortran
compilers tend to agree on certain things. So we follow gfortran's lead for
semantics. The following general rules exist:

  * The size in bytes of a stored value is equal to its type's kind value. For
    example, a @REAL(4)@ takes 4 bytes. In general, for any type, only powers of
    2 are ever valid kinds.
  * Different types have different permitted kind values. This is what prevents
    us from simply carrying around a type name and a kind. For example, in our
    representation (and most in use), @REAL(2)@ isn't a valid type, while
    @INTEGER(2)@ is.
-}

{- $naming-conventions

To prevent clashes with common Haskell types and definitions, most
representation types are prefixed with @F@, read as _Fortran_.

-}

module FortranSrc.Repr
  (
  -- * Design aims
  -- $design-aims

  -- * Kind semantics
  -- $kind-semantics

  -- * Naming conventions
  -- $naming-conventions
  ) where

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
interactions and precision requirements. In general, we follow gfortran's lead
for semantics. The following general rules exist:

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

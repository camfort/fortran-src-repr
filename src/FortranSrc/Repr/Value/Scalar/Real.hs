{-# LANGUAGE TypeFamilyDependencies #-} -- used for better inference (maybe)

module FortranSrc.Repr.Value.Scalar.Real where

import FortranSrc.Repr.Value.Common
import FortranSrc.Repr.Type.Scalar.Real
import Data.Singletons

data FReal (pr :: PrimRepr) (k :: FTReal) where
    FReal4 :: Float  -> FReal pr 'FTReal4
    FReal8 :: Double -> FReal pr 'FTReal8
deriving stock instance Show (FReal pr k)

data SomeFReal pr
  = forall (k :: FTReal). SomeFReal (Sing k) (FReal pr k)
deriving stock instance Show (SomeFReal pr)

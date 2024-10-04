module Test.Propagation where

import Pantograph.Grammar
import Pantograph.Tree
import Prelude

import Pantograph.EitherF (EitherF(..))
import Pantograph.Example.Slc as Slc
import Pantograph.Propagation (fromPropagDerivToDeriv, propagateFixpoint)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

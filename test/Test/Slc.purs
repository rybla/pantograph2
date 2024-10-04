module Test.Slc where

import Pantograph.Example.Slc
import Pantograph.Grammar
import Pantograph.Tree
import Prelude hiding (zero)

import Pantograph.Propagation (fromPropagDerivToDeriv, propagateFixpoint)
import Test.Common (shouldEqual_pretty)
import Test.Spec (Spec, describe, it, pending)

spec :: Spec Unit
spec = describe "Slc" do
  it "#1" do
    shouldEqual_pretty
      ( fromPropagDerivToDeriv $ propagateFixpoint propagRules
          (lam' nil (ref' (ext nil) (zero' nil)))
      )
      ( pure $
          (lam nil (ref (ext nil) (zero nil)))
      )


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
  it "trivial" do
    shouldEqual_pretty
      ( fromPropagDerivToDeriv $ propagateFixpoint propagRules
          -- (lam' nil (ref' (ext nil) (zero' nil)))
          (lam' ?a ?a)
      )
      ( pure $
          -- (lam nil (ref (ext nil) (zero nil)))
          ?a
      )
  it "one step" do
    pure unit -- TODO

module Test.Slc where

import Pantograph.Example.Slc
import Pantograph.Grammar
import Pantograph.Tree
import Pantograph.Propagation
import Prelude hiding (zero)

import Test.Common (shouldEqual_pretty)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = describe "Slc" do
  it "trivial" do
    shouldEqual_pretty
      ( fromPropagDerivToDeriv $ propagateFixpoint propagRules $
          lamₚ nil (refₚ (ext nil) (zeroₚ nil))
      )
      ( pure $
          lam nil (ref (ext nil) (zero nil))
      )
  it "one step" do
    shouldEqual_pretty
      ( fromPropagDerivToDeriv $ propagateFixpoint propagRules $
          lamₚ nil (refₚ (ext nil) (zeroₚ nil)) ↓ (ext₀ ▵∂+ id nil)
      )
      ( pure $
          lam nil (ref (ext nil) (zero nil))
      )

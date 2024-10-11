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
      (lam nil (ref (ext nil) (zero nil)))
      ( fromPropagDerivToDeriv $ propagateFixpoint propagRules $
          lam_p nil (ref_p (ext nil) (zero_p nil))
      )
  it "one step" do
    shouldEqual_pretty
      (lam nil (ref (ext nil) (zero nil)))
      ( fromPropagDerivToDeriv $ propagateFixpoint propagRules $
          term_c (ext_0 %∂+ id nil) ↓ lam_p nil (ref_p (ext nil) (zero_p nil))
      )

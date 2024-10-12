module Test.Slc where

import Pantograph.Example.Slc
import Pantograph.Grammar
import Pantograph.Propagation
import Pantograph.Tree
import Prelude hiding (zero)

import Pantograph.Pretty (pretty)
import Test.Common (shouldEqual_pretty)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = describe "Slc" do
  shouldEqual_propagateFixpoint
    (lam emp (ref (ext emp) (zero emp)))
    (lam_p emp (ref_p (ext emp) (zero_p emp)))

  shouldEqual_propagateFixpoint
    (ref (ext emp) (zero emp))
    (term_c (ext_0 %∂- id emp) ↓ ref_p (ext emp) (zero_p emp))

  shouldEqual_propagateFixpoint
    (ref (ext (ext emp)) (suc (ext (ext emp)) (zero emp)))
    (term_c (ext_0 %∂+ id (ext emp)) ↓ ref_p (ext emp) (zero_p emp))

shouldEqual_propagateFixpoint d pd =
  it (pretty pd) $
    shouldEqual_pretty
      d
      (fromPropagDerivToDeriv $ propagateFixpoint propagRules $ pd)

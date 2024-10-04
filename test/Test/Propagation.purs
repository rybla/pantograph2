module Test.Propagation where

import Prelude

import Pantograph.Example.Slc as Slc
import Pantograph.Propagation (fromPropagDerivToDeriv, propagateFixpoint)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Propagation" do
  pending "test some specific resolutions of propagation to make sure i've got the right manual/auto rules"
  it "#1" do
    shouldEqual
      ( fromPropagDerivToDeriv $ propagateFixpoint Slc.propagRules $
          ?a
      )
      ?A
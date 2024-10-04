module Test.Main where

import Prelude

import Effect (Effect)
import Test.Propagation as Propagation
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Slc as Test.Slc

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Test.Slc.spec


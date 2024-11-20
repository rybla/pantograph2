module Test.Main where

import Prelude

import Effect (Effect)
import Test.Slc as Test.Slc
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

spec = do
  Test.Slc.spec

module Main where

import Prelude

import Control.Monad.Cont (callCC, runCont, runContT)
import Effect (Effect)
import Effect.Class.Console (log)
import Pantograph.Example.Ulc as Ulc
import Pantograph.Pretty (pretty)

main :: Effect Unit
main = do
  -- log (pretty (Ulc.derivRules Lam_D))
  pure unit
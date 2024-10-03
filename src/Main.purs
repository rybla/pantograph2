module Main where

import Prelude

import Control.Monad.Cont (callCC, runCont, runContT)
import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  -- flip runContT (\_unit -> log "running continuation") do
  --   s :: String <- callCC (_ $ ?a)
  --   -- log $ "result of callCC: " <> s
  --   -- s <- callCC (_ $ "2")
  --   -- s <- callCC (\k -> pure "2")
  --   pure unit

  let
    result :: String
    result = flip runCont show do
      x <- callCC (_ $ false)
      if x then
        pure "1"
      else
        pure "2"
  log result


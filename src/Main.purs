module Main where

import Prelude

import Control.Monad.Cont (callCC, runCont, runContT)
import Effect (Effect)
import Effect.Class.Console (log)
import Pantograph.Pretty (pretty)
import Prim.Row (class Union)
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = do
  -- log (pretty (Ulc.derRules Lam_D))
  pure unit

ex1 :: forall a. Union a a a => Proxy a -> Record a -> Record a
ex1 _ = identity

-- ex1_ = ex1 (Proxy :: Proxy (x :: Int))
module Pantograph.Editor where

import Prelude

import Data.Maybe (Maybe)
import Pantograph.Grammar

newtype Editor d s = Editor
  { deriv :: Deriv d s
  , derivRules :: DerivRules d s
  , propagRules :: PropagRules d s
  , canonicalDerivOfSort :: Sort s -> Maybe (Deriv d s)
  }

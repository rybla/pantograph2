module Pantograph.EditorBackend where

import Data.Maybe (Maybe)
import Pantograph.Grammar (Deriv, DerivRules, PropagRules, Sort)

newtype EditorBackend d s = EditorBackend
  { derivRules :: DerivRules d s
  , propagRules :: PropagRules d s
  , canonicalDerivOfSort :: Sort s -> Maybe (Deriv d s)
  }

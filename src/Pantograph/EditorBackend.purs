module Pantograph.EditorBackend where

import Data.Maybe (Maybe)
import Pantograph.Grammar (DerivRules, PropagRules, SortLabel, DerivLabel)
import Pantograph.Tree (Tree)

newtype EditorBackend d s = EditorBackend
  { derivRules :: DerivRules d s
  , propagRules :: PropagRules d s
  , canonicalDerivOfSort :: Tree (SortLabel s) -> Maybe (Tree (DerivLabel d s))
  }

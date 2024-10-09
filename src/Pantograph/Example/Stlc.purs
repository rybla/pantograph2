-- | Simply typed lambda calculus
module Pantograph.Example.Stlc where

import Pantograph.Grammar
import Pantograph.Tree
import Prelude

import Data.Maybe (Maybe)
import Pantograph.Utility (unimplemented)

data S

data D

--------------------------------------------------------------------------------

derivRules :: DerivRules D S
derivRules _ = unimplemented "derivRules"

propagRules :: PropagRules D S
propagRules = mempty

canonicalDerivOfSort :: Tree (SortLabel S) -> Maybe (Tree (DerivLabel D S))
canonicalDerivOfSort _ = unimplemented "canonicalDerivOfSort"


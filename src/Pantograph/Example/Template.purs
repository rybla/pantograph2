module Pantograph.Example.Template where

import Pantograph.Grammar
import Prelude

import Data.Maybe (Maybe)
import Pantograph.Utility (unimplemented)

data S

data D

--------------------------------------------------------------------------------

derivRules :: DerivRules D S
derivRules _ = unimplemented "derivRules"

propagRules :: PropagRules D S
propagRules = []

canonicalDerivOfSort :: Sort S -> Maybe (Deriv D S)
canonicalDerivOfSort _ = unimplemented "canonicalDerivOfSort"


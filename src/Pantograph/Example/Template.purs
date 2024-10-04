module Pantograph.Example.Template where

import Pantograph.Grammar
import Pantograph.Tree
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Pantograph.Pretty (class Pretty)
import Pantograph.Utility (unimplemented)

data S = S

derive instance Generic S _

instance Show S where
  show x = genericShow x

instance Pretty S where
  pretty S = unimplemented "(Pretty S).pretty"

data D = D

derive instance Generic D _

instance Show D where
  show x = genericShow x

instance Pretty D where
  pretty D = unimplemented "(Pretty D).pretty"

--------------------------------------------------------------------------------

derivRules :: DerivRules D S
derivRules _ = unimplemented "derivRules"

propagRules :: PropagRules D S
propagRules = mempty

canonicalDerivOfSort :: Sort S -> Maybe (Deriv D S)
canonicalDerivOfSort _ = unimplemented "canonicalDerivOfSort"


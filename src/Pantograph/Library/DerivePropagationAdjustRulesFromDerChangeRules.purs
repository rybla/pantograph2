module Pantograph.Library.DerivePropagationAdjustRulesFromDerChangeRules where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Pantograph.Language (class IsLanguage, AdjustRules, SortLabel)
import Pantograph.Tree (ChangeLabel, Tree)
import Pantograph.Utility (todo)

--------------------------------------------------------------------------------
-- DerChangeRule 
--------------------------------------------------------------------------------

newtype DerChangeRule s = DerChangeRule
  { kids :: List { change :: Tree (ChangeLabel (SortLabel s)) } }

derive instance Generic (DerChangeRule s) _

instance Show s => Show (DerChangeRule s) where
  show x = genericShow x

instance Eq s => Eq (DerChangeRule s) where
  eq x = genericEq x

derive instance Functor DerChangeRule

type DerChangeRules d s = d -> DerChangeRule s

class HasDerChangeRules d s | d -> s where
  derChangeRules :: DerChangeRules d s

--------------------------------------------------------------------------------

propagationAdjustRules :: forall d s. HasDerChangeRules d s => AdjustRules d s
propagationAdjustRules = mempty -- TODO: figure out how to derive these from DerChangeRules


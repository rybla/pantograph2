module Pantograph.Library.DerivePropagationAdjustRulesFromDerRules where

import Prelude

import Pantograph.Language (class IsLanguage, AdjustRules)

propagationAdjustRules :: forall d s. IsLanguage d s => AdjustRules d s
propagationAdjustRules = mempty -- TODO: figure out how to derive these from DerRules


module Pantograph.Library.PropagRules where

import Pantograph.Grammar
import Prelude

import Control.Plus (empty)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Pantograph.Tree (ChangeLabel(..), Tree(..), leftEndpoint)
import Pantograph.Utility (assertM, todo, unimplemented)

defaultPropagRules :: forall d s. Eq s => DerivRules d s -> PropagRules d s
defaultPropagRules = pure >>> apply
  ( [ defaultDownPropagRule
    , defaultUpPropagRule
    ]
      # List.fromFoldable
  )

defaultDownPropagRule :: forall d s. Eq s => DerivRules d s -> PropagRule d s
defaultDownPropagRule derivRules = PropagRule "DefaultDown" \_mb_th -> case _ of
  -- Tree (PropagBoundary Down ch) (Tree (Inject_PropagDerivLabel dl@(DerivLabel d sigma_d)) kids : Nil) -> do
  --   {-
  --   DerivRule d
  --     ... ch_rule_kid_i ...
  --     ------------------------
  --     sort_rule

  --   --------------------------------------------------
  --   { (d , sigma_d) ▵ ... kid_i ... }↓{ch}
  --   ~~>
  --   { (d, sigma_d) ▵ ... {kid_i}↓{TODO} }↑{TODO}

  --   -}
  --   todo ""
  -- Tree (PropagBoundary Down ch) (Tree (Inject_PropagDerivLabel dl@(DerivLabel d sigma_d)) kids : Nil) -> do
  --   {-
  --   DerivRule d:
  --     ... kid_rule_i ...
  --     ------------------------
  --     sort_rule

  --   ---------------------------------------------------------------------
  --   {( d , sigma_d , ... kid_i ... )}↓{ch}
  --   ~~>
  --   {( d , sigma_d , ... {kid_i}↓{sigma_uni kid_rule_i} ... )}↑{ch_uni}
  --   -}
  --   let DerivRule _name kids_rule sort_rule = derivRules d
  --   let sort = applyRulialVarSubst sigma_d (sort_rule # map (map pure)) :: MetaSort s
  --   assertM "a PropagBoundary's Change's left endpoint must match the Sort of the inner Deriv"
  --     (leftEndpoint ch == sort)
  --   sigma_uni /\ ch_uni <- divideMetaSortChangeByRulialSortChange (sort_rule # map Congruence) ch
  --   pure
  --     $ Tree (PropagBoundary Up ch_uni)
  --     $ pure
  --     $ Tree (Inject_PropagDerivLabel dl)
  --     $ (\f -> List.zipWith f kids kids_rule)
  --         \kid ruleKid ->
  --           Tree (PropagBoundary Down (ruleKid # map (map (Congruence >>> map pure)) # applyRulialVarSubst sigma_uni))
  --             (pure kid)
  _ -> empty

defaultUpPropagRule :: forall d s. Eq s => DerivRules d s -> PropagRule d s
defaultUpPropagRule _derivRules = unimplemented "defaultUpPropagRule"

-- | Given 
-- | ```
-- | ch1 : RulialSortChange
-- | ch2 : MetaSortChange
-- | ```
-- | tries to find
-- | ```
-- | ch    : MetaSortChange s
-- | sigma : RulialVarSubst (MetaSortChange s)
-- | ```
-- | such that
-- | ```
-- | sigma ch1 = ch2 ∘ ch
-- | ```
-- | You can kinda think of this as `ch = sigma ch1 / ch2`
divideMetaSortChangeByRulialSortChange
  :: forall s
   . RulialSortChange s
  -> MetaSortChange s
  -> Maybe (RulialVarSubst (MetaSortChange s) /\ MetaSortChange s)
divideMetaSortChangeByRulialSortChange _ch1 _ch2 = todo "unifyChangeWithRulialChange"

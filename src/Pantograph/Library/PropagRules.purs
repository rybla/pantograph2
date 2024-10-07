module Pantograph.Library.PropagRules where

import Pantograph.Grammar
import Prelude

import Control.Plus (empty)
import Data.Functor.Compose (Compose(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Pantograph.EitherF (EitherF(..))
import Pantograph.Tree (Tree(..), id, innerEndpoint)
import Pantograph.Unification (unifyMetaSorts)
import Pantograph.Utility (todo, unimplemented)

defaultPropagRules :: forall d s. Eq s => DerivRules d s -> PropagRules d s
defaultPropagRules = pure >>> apply
  ( [ defaultDownPropagRule
    , defaultUpPropagRule
    ] # List.fromFoldable
  )

defaultDownPropagRule :: forall d s. Eq s => DerivRules d s -> PropagRule d s
defaultDownPropagRule derivRules = PropagRule "DefaultDown" \_mb_th -> case _ of
  {-
  DerivRule d
    ... ch_rule_kid_i ...
    ------------------------
    sort_rule

  --------------------------------------------------
  { (d , sigma_d) ▵ ... kid_i ... }↓{ch}
  ~~>
  { (d, sigma_d) ▵ ... {kid_i}↓{TODO} }↑{TODO}

  -}
  Tree (LeftF (Compose (PropagBoundary Down ch))) (Tree (RightF (Compose dl@(DerivLabel d sigma_d))) kids : Nil) -> do
    -- (1) for a d', matches wrap pattern if down change unifies with the
    -- reverse of the change to one of the kids (skip kid if the parent-kid
    -- change is id)
    todo ""
  Tree (LeftF (Compose (PropagBoundary Down ch))) (Tree (RightF (Compose dl@(DerivLabel d sigma_d))) kids : Nil) -> do
    -- (2) matches unwrap pattern if down change unifies with change to one of
    -- the kids (skip kid if the parent-kid change is id)
    todo ""
  Tree (LeftF (Compose (PropagBoundary Down ch))) (Tree (RightF (Compose dl)) kids : Nil) -> do
    -- otherwise, use congruence pattern (unify with sort_rule, then compose
    -- with change from parent to kid and propagate that change as a boundary on
    -- each kid)
    -- getParentMetaSortOfDerivLabel :: ∀ d s. DerivRules d s → DerivLabel d s → MetaSort s
    -- sigma <- unifyMetaSorts (ch # ?a # innerEndpoint) (dl # getParentMetaSortOfDerivLabel derivRules # ?a)
    let ch' = ?ch # applyMetaVarSubst ?sigma
    pure ?a
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

module Pantograph.Library.PropagRules where

import Pantograph.Grammar
import Pantograph.Tree
import Prelude

import Control.Plus (empty)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (fromMaybe')
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Pantograph.Unification (unifyMetaSortChanges)
import Pantograph.Utility (bug)

defaultPropagRules :: forall d s. Eq s => DerivRules d s -> PropagRules d s
defaultPropagRules = pure >>> apply
  ( [ defaultCongruenceDownPropagRule
    ] # List.fromFoldable
  )

defaultCongruenceDownPropagRule :: forall d s. Eq s => DerivRules d s -> PropagRule d s
defaultCongruenceDownPropagRule derivRules = PropagRule "defaultCongruenceDownPropagRule" \_mb_th -> case _ of
  PropagBoundary Down ch ▵ ((Inject_PropagDerivLabel dl ▵ kids) : Nil) -> do
    let chs_kids = getKidMetaSortChangesOfDerivLabel derivRules dl
    kids' <-
      List.zip kids chs_kids # traverse \(kid /\ ch_kid) -> do
        ch' <- ch `composeChanges'` ch_kid
        pure $ PropagBoundary Down ch' ▵ (kid : Nil)
    pure $ Inject_PropagDerivLabel dl ▵ kids'
  _ -> empty

-- defaultUnwrapDownPropagRule :: forall d s. Eq s => DerivRules d s -> PropagRule d s
-- defaultUnwrapDownPropagRule derivRules = PropagRule "defaultUnwrapDownPropagRule" \_mb_th -> case _ of
--   PropagBoundary Down ch ▵ ((Inject_PropagDerivLabel dl ▵ kids) : Nil) -> do
--     let chs_kids = getKidMetaSortChangesOfDerivLabel derivRules dl
--     -- match ch with an inverted ch_kid in chs_kids
--     (i_kid /\ ch_kid) /\ sigma_ch <- chs_kids # mapWithIndex Tuple # List.findMap (\(i /\ ch_kid) -> ((i /\ ch_kid) /\ _) <$> unifyMetaSortChanges ch ch_kid)
--     let kid = kids List.!! i_kid # fromMaybe' \_ -> bug "i_kid out of bounds"
--     let ch' = dl # getParentMetaSortOfDerivLabel derivRules # map (map Congruence) # applyMetaVarSubst sigma_ch
--     -- let ch'' =
--     -- need to compose ch_kid with ch somehow, after applying sigma_ch to ch_kid
--     pure $ PropagBoundary Down ch' ▵ (kid : Nil)
--   _ -> empty

-- defaultPropagRules :: forall d s. Eq s => DerivRules d s -> PropagRules d s
-- defaultPropagRules = pure >>> apply
--   ( [ defaultDownPropagRule
--     , defaultUpPropagRule
--     ] # List.fromFoldable
--   )

-- defaultDownPropagRule :: forall d s. Eq s => DerivRules d s -> PropagRule d s
-- defaultDownPropagRule derivRules = PropagRule "DefaultDown" \_mb_th -> case _ of
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
--   Tree (PropagBoundary Down ch) (Tree (Inject_PropagDerivLabel dl@(DerivLabel d sigma_d)) kids : Nil) -> do
--     -- (1) for a d', matches wrap pattern if down change unifies with the
--     -- reverse of the change to one of the kids (skip kid if the parent-kid
--     -- change is id)
--     todo ""
--   Tree (PropagBoundary Down ch) (Tree (Inject_PropagDerivLabel dl@(DerivLabel d sigma_d)) kids : Nil) -> do
--     -- (2) matches unwrap pattern if down change unifies with change to one of
--     -- the kids (skip kid if the parent-kid change is id)
--     todo ""
--   Tree (PropagBoundary Down ch) (Tree (Inject_PropagDerivLabel dl) kids : Nil) -> do
--     -- otherwise, use congruence pattern (unify with sort_rule, then compose
--     -- with change from parent to kid and propagate that change as a boundary on
--     -- each kid)
--     -- getParentMetaSortOfDerivLabel :: ∀ d s. DerivRules d s → DerivLabel d s → MetaSort s
--     sigma :: MetaVarSubst (MetaSort s) <-
--       unifyMetaSorts (ch # innerEndpoint') (dl # getParentMetaSortOfDerivLabel derivRules)
--     let ch' = ch # applyMetaVarSubst (sigma # map (map (map Congruence)))
--     let ch'' = composeChanges ?ch' ?a
--     pure ?a
--   _ -> empty

-- defaultUpPropagRule :: forall d s. Eq s => DerivRules d s -> PropagRule d s
-- defaultUpPropagRule _derivRules = unimplemented "defaultUpPropagRule"

-- -- | Given 
-- -- | ```
-- -- | ch1 : RulialSortChange
-- -- | ch2 : MetaSortChange
-- -- | ```
-- -- | tries to find
-- -- | ```
-- -- | ch    : MetaSortChange s
-- -- | sigma : RulialVarSubst (MetaSortChange s)
-- -- | ```
-- -- | such that
-- -- | ```
-- -- | sigma ch1 = ch2 ∘ ch
-- -- | ```
-- -- | You can kinda think of this as `ch = sigma ch1 / ch2`
-- divideMetaSortChangeByRulialSortChange
--   :: forall s
--    . RulialSortChange s
--   -> MetaSortChange s
--   -> Maybe (RulialVarSubst (MetaSortChange s) /\ MetaSortChange s)
-- divideMetaSortChangeByRulialSortChange _ch1 _ch2 = todo "unifyChangeWithRulialChange"

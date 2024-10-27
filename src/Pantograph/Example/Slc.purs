module Pantograph.Example.Slc where

import Pantograph.Language
import Pantograph.Tree
import Prelude

import Control.Alternative (empty)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Show.Generic (genericShow)
import Pantograph.Pretty (class Pretty)
import Pantograph.Utility (bug)
import Type.Proxy (Proxy(..))

data S
  = Emp
  | Ext
  | Var
  | Term

derive instance Generic S _

instance Show S where
  show x = genericShow x

instance PrettyTreeLabel S where
  prettyTree Emp Nil = "∅"
  prettyTree Ext (gamma : Nil) = "E" <> gamma
  prettyTree Var (gamma : Nil) = "Var " <> gamma
  prettyTree Term (gamma : Nil) = "Term " <> gamma
  prettyTree _ _ = bug "invalid S"

instance Eq S where
  eq x = genericEq x

instance Pretty S where
  pretty = show

instance IsSortRuleLabel S

data D
  = Zero
  | Suc
  | Free
  | Ref
  | Lam
  | App
  | Hole

derive instance Generic D _

instance Show D where
  show x = genericShow x

instance Eq D where
  eq x = genericEq x

instance Pretty D where
  pretty = show

instance PrettyTreeLabel D where
  prettyTree Zero Nil = "Z"
  prettyTree Suc (n : Nil) = "S" <> n
  prettyTree Ref (x : Nil) = "#" <> x
  prettyTree Lam (b : Nil) = "(λ " <> b <> ")"
  prettyTree App (f : a : Nil) = "(" <> f <> " " <> a <> ")"
  prettyTree Hole Nil = "?"
  prettyTree _ _ = bug "invalid D"

instance IsDerivRuleLabel D

instance HasDerivRules D S where
  derivRules Zero = DerivRule
    { sort: Var %|^ [ Ext %|^ [ gamma ] ]
    , kids: mempty
    }
    where
    gamma = mkRulialVar "gamma"
  derivRules Suc = DerivRule
    { sort: Var %|^ [ gamma ]
    , kids: List.fromFoldable
        [ { sort: Var %|^ [ Ext %|^ [ gamma ] ] } ]
    }
    where
    gamma = mkRulialVar "gamma"
  derivRules Free = DerivRule
    { sort: Var %|^ [ gamma ]
    , kids: List.fromFoldable
        [ { sort: Var %|^ [ Ext %|^ [ gamma ] ] } ]
    }
    where
    gamma = mkRulialVar "gamma"
  derivRules Ref = DerivRule
    { sort: Term %|^ [ gamma ]
    , kids: List.fromFoldable
        [ { sort: Var %|^ [ gamma ] } ]
    }
    where
    gamma = mkRulialVar "gamma"
  derivRules Lam = DerivRule
    { sort: Term %|^ [ gamma ]
    , kids: List.fromFoldable
        [ { sort: Term %|^ [ Ext %|^ [ gamma ] ] } ]
    }
    where
    gamma = mkRulialVar "gamma"
  derivRules App = DerivRule
    { sort: Term %|^ [ gamma ]
    , kids: List.fromFoldable
        [ { sort: Term %|^ [ gamma ] }
        , { sort: Term %|^ [ gamma ] }
        ]
    }
    where
    gamma = mkRulialVar "gamma"
  derivRules Hole = DerivRule
    { sort: Term %|^ [ gamma ]
    , kids: mempty
    }
    where
    gamma = mkRulialVar "gamma"

instance HasDerivPropagRules D S where
  derivPropagRules Zero = DerivPropagRule
    { kids: mempty
    }
  derivPropagRules Suc = DerivPropagRule
    { kids: List.fromFoldable
        [ { passthrough_down: matchTreeChangeSort
              (Var %|∂.^ [ Ext %|∂.^ [ _gamma ] ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                Var %∂.^ [ gamma ]
          , passthrough_up: matchTreeChangeSort
              (Var %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                Var %∂.^ [ Ext %∂.^ [ gamma ] ]
          , unwrap_down: matchTreeChangeSort
              (Var %|∂.^ [ Ext %|∂-^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                { up: id $ Var %^ [ gamma # outerEndpoint ]
                , down: Var %∂.^ [ gamma ]
                }
          , unwrap_up: matchTreeChangeSort
              (Var %|∂.^ [ Ext %|∂+^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                { up: Var %∂.^ [ Ext %∂.^ [ gamma ] ]
                , down: id $ Var %^ [ gamma # innerEndpoint ]
                }
          , wrap_down: matchTreeChangeSort
              (Var %|∂.^ [ Ext %|∂+^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                { up: id $ Var %^ [ Ext %^ [ gamma # outerEndpoint ] ]
                , down: Var %∂.^ [ gamma ]
                }
          , wrap_up: matchTreeChangeSort
              (Var %|∂.^ [ Ext %|∂+^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                { up: Var %∂.^ [ Ext %∂.^ [ gamma ] ]
                , down: id $ Var %^ [ gamma # innerEndpoint ]
                }
          }
        ]
    }
    where
    _gamma = matchialVar (Proxy :: Proxy "gamma")
  derivPropagRules Free = DerivPropagRule
    { kids: List.fromFoldable
        [ { passthrough_down: matchTreeChangeSort
              -- TODO: is this right?
              (Var %|∂.^ [ Ext %|∂.^ [ _gamma ] ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                Var %∂.^ [ gamma ]
          , passthrough_up: matchTreeChangeSort
              -- TODO: is this right?
              (Var %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                Var %∂.^ [ Ext %∂.^ [ gamma ] ]
          , unwrap_down: matchTreeChangeSort
              (Var %|∂.^ [ Ext %|∂-^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                { up: id $ Var %^ [ gamma # outerEndpoint ]
                , down: Var %∂.^ [ gamma ]
                }
          , unwrap_up: matchTreeChangeSort
              (Var %|∂.^ [ Ext %|∂+^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                { up: Var %∂.^ [ Ext %∂.^ [ gamma ] ]
                , down: id $ Var %^ [ gamma # innerEndpoint ]
                }
          , wrap_down: matchTreeChangeSort
              (Var %|∂.^ [ Ext %|∂+^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                { up: id $ Var %^ [ Ext %^ [ gamma # outerEndpoint ] ]
                , down: Var %∂.^ [ gamma ]
                }
          , wrap_up: matchTreeChangeSort
              (Var %|∂.^ [ Ext %|∂+^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                { up: Var %∂.^ [ Ext %∂.^ [ gamma ] ]
                , down: id $ Var %^ [ gamma # innerEndpoint ]
                }
          }
        ]
    }
    where
    _gamma = matchialVar (Proxy :: Proxy "gamma")
  derivPropagRules Ref = DerivPropagRule
    { kids: List.fromFoldable
        [ { passthrough_down: matchTreeChangeSort
              (Term %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                Var %∂.^ [ gamma ]
          , passthrough_up: matchTreeChangeSort
              (Var %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                Term %∂.^ [ gamma ]
          , unwrap_down: const empty
          , unwrap_up: const empty
          , wrap_down: const empty
          , wrap_up: const empty
          }
        ]
    }
    where
    _gamma = matchialVar (Proxy :: Proxy "gamma")
  derivPropagRules Lam = DerivPropagRule
    { kids: List.fromFoldable
        [ { passthrough_down: matchTreeChangeSort
              (Term %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                Term %∂.^ [ Ext %∂.^ [ gamma ] ]
          , passthrough_up: matchTreeChangeSort
              (Term %|∂.^ [ Ext %|∂.^ [ _gamma ] ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                Term %∂.^ [ gamma ]
          , unwrap_down: const empty
          , unwrap_up: const empty
          , wrap_down: const empty
          , wrap_up: const empty
          }
        ]
    }
    where
    _gamma = matchialVar (Proxy :: Proxy "gamma")
  derivPropagRules App = DerivPropagRule
    { kids: List.fromFoldable
        [ { passthrough_down: matchTreeChangeSort
              (Term %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                Term %∂.^ [ gamma ]
          , passthrough_up: matchTreeChangeSort
              (Term %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                Term %∂.^ [ gamma ]
          , unwrap_down: const empty
          , unwrap_up: const empty
          , wrap_down: const empty
          , wrap_up: const empty
          }
        , { passthrough_down: matchTreeChangeSort
              (Term %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                Term %∂.^ [ gamma ]
          , passthrough_up: matchTreeChangeSort
              (Term %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _))
              \{ gamma } ->
                Term %∂.^ [ gamma ]
          , unwrap_down: const empty
          , unwrap_up: const empty
          , wrap_down: const empty
          , wrap_up: const empty
          }
        ]
    }
    where
    _gamma = matchialVar (Proxy :: Proxy "gamma")
  derivPropagRules Hole = DerivPropagRule
    { kids: mempty
    }

instance IsLanguage D S


{-
one major limitation here is that you can't re-nest lambdas and have the
referenced variables recover -- since there isn't a way to tell that the
re-nested lambda corresponds to the new position that the free variables should
take.
-}
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
import Pantograph.Utility (bug, todo)
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
  prettyTree Ext (g : Nil) = "E" <> g
  prettyTree Var (g : Nil) = "Var " <> g
  prettyTree Term (g : Nil) = "Term " <> g
  prettyTree _ _ = bug "invalid S"

instance Eq S where
  eq x = genericEq x

instance Pretty S where
  pretty = show

instance IsSortRuleLabel S

data D
  = Free
  | Zero
  | Suc
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
  prettyTree Free Nil = "F"
  prettyTree Zero Nil = "Z"
  prettyTree Suc (n : Nil) = "S" <> n
  prettyTree Ref (x : Nil) = "#" <> x
  prettyTree Lam (b : Nil) = "(λ " <> b <> ")"
  prettyTree App (f : a : Nil) = "(" <> f <> " " <> a <> ")"
  prettyTree Hole Nil = "?"
  prettyTree _ _ = bug "invalid D"

instance IsDerivRuleLabel D

-- instance HasDerivRules D S where
--   derivRules VarStub = DerivRule
--     { sort: ?a
--     }
--   -- derivRules ZeroWeak = DerivRule
--   --   { sort: Var %|^ [ Emp %|^ [] ]
--   --   , kids: mempty
--   --   }
--   -- derivRules SucWeak = DerivRule
--   --   { sort: Var %|^ [ gamma ]
--   --   , kids: List.fromFoldable
--   --       [ { sort: Var %|^ [ Ext %|^ [ gamma ] ] } ]
--   --   }
--   --   where
--   --   gamma = mkRulialVar "gamma"
--   derivRules Free = DerivRule
--     { sort: Var %|^ [ gamma ]
--     , kids: List.fromFoldable
--         [ { sort: Var %|^ [ Ext %|^ [ gamma ] ] } ]
--     }
--     where
--     gamma = mkRulialVar "gamma"
--   derivRules Zero = DerivRule
--     { sort: Var %|^ [ Ext %|^ [ gamma ] ]
--     , kids: List.fromFoldable
--         [ { sort: Var %|^ [ gamma ] } ]
--     }
--     where
--     gamma = mkRulialVar "gamma"
--   derivRules Suc = DerivRule
--     { sort: Var %|^ [ gamma ]
--     , kids: List.fromFoldable
--         [ { sort: Var %|^ [ Ext %|^ [ gamma ] ] } ]
--     }
--     where
--     gamma = mkRulialVar "gamma"
--   derivRules Ref = DerivRule
--     { sort: Term %|^ [ gamma ]
--     , kids: List.fromFoldable
--         [ { sort: Var %|^ [ gamma ] } ]
--     }
--     where
--     gamma = mkRulialVar "gamma"
--   derivRules Lam = DerivRule
--     { sort: Term %|^ [ gamma ]
--     , kids: List.fromFoldable
--         [ { sort: Term %|^ [ Ext %|^ [ gamma ] ] } ]
--     }
--     where
--     gamma = mkRulialVar "gamma"
--   derivRules App = DerivRule
--     { sort: Term %|^ [ gamma ]
--     , kids: List.fromFoldable
--         [ { sort: Term %|^ [ gamma ] }
--         , { sort: Term %|^ [ gamma ] }
--         ]
--     }
--     where
--     gamma = mkRulialVar "gamma"
--   derivRules Hole = DerivRule
--     { sort: Term %|^ [ gamma ]
--     , kids: mempty
--     }
--     where
--     gamma = mkRulialVar "gamma"

-- instance IsLanguage D S

-- instance HasDerivChangeRules D S where
--   derivChangeRules ZeroWeak = todo "derivChangeRules ZeroWeak"
--   derivChangeRules SucWeak = todo "derivChangeRules SucWeak"
--   derivChangeRules Free = todo "derivChangeRules Free"
--   derivChangeRules Zero = todo "derivChangeRules Zero"
--   derivChangeRules Suc = todo "derivChangeRules Suc"
--   derivChangeRules Ref = todo "derivChangeRules Ref"
--   derivChangeRules Lam = todo "derivChangeRules Lam"
--   derivChangeRules App = todo "derivChangeRules App"
--   derivChangeRules Hole = todo "derivChangeRules Hole"

-- instance IsDerivChangeLanguage D S

-- instance HasDerivPropagRules D S where
--   derivPropagRules ZeroWeak = DerivPropagRule
--     { kids: mempty }
--   derivPropagRules SucWeak = DerivPropagRule
--     { kids: List.fromFoldable
--         [ { passthrough_down: matchTreeChangeSort (Var %|∂.^ [ Ext %|∂.^ [ _gamma ] ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> Var %∂.^ [ gamma ]
--           , passthrough_up: matchTreeChangeSort (Var %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> Var %∂.^ [ Ext %∂.^ [ gamma ] ]
--           , unwrap_down: matchTreeChangeSort (Var %|∂.^ [ Ext %|∂-^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> { up: empty, down: pure $ Var %∂.^ [ gamma ] }
--           , unwrap_up: matchTreeChangeSort (Var %|∂.^ [ Ext %|∂+^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> { up: pure $ Var %∂.^ [ Ext %∂.^ [ gamma ] ], down: empty }
--           , wrap_down: matchTreeChangeSort (Var %|∂.^ [ Ext %|∂+^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> { up: empty, down: pure $ Var %∂.^ [ gamma ] }
--           , wrap_up: matchTreeChangeSort (Var %|∂.^ [ Ext %|∂+^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> { up: pure $ Var %∂.^ [ Ext %∂.^ [ gamma ] ], down: empty }
--           }
--         ]
--     }
--     where
--     _gamma = matchialVar (Proxy :: Proxy "gamma")
--   derivPropagRules Free = DerivPropagRule
--     { kids: List.fromFoldable
--         [ { passthrough_down: matchTreeChangeSort (Var %|∂.^ [ Ext %|∂.^ [ _gamma ] ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> Var %∂.^ [ gamma ]
--           , passthrough_up: matchTreeChangeSort (Var %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> Var %∂.^ [ Ext %∂.^ [ gamma ] ]
--           -- when Free unwraps via a down change, then sends up a change that
--           -- will cause a Zero to wrap
--           , unwrap_down: matchTreeChangeSort
--               (Var %|∂.^ [ Ext %|∂-^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _))
--               \{ gamma } ->
--                 { up: pure $ Var %∂.^ [ ExtFree %∂-^ [] << (gamma # outerEndpoint # id) >> [] ]
--                 , down: pure $ Var %∂.^ [ gamma ]
--                 }
--           , unwrap_up: const empty
--           , wrap_down: const empty
--           -- matches on an ExtFree sent up by the result of unwrapping a Zero
--           , wrap_up: matchTreeChangeSort
--               (Var %|∂.^ [ ExtFree %|∂+^ [] << _gamma >> [] ])
--               \{ gamma } ->
--                 { up: pure $ Var %∂.^ [ gamma ]
--                 , down: empty
--                 }
--           }
--         ]
--     }
--     where
--     _gamma = matchialVar (Proxy :: Proxy "gamma")
--   derivPropagRules Zero = DerivPropagRule
--     { kids: List.fromFoldable
--         [ { passthrough_down: matchTreeChangeSort (Var %|∂.^ [ Ext %|∂.^ [ _gamma ] ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> Var %∂.^ [ gamma ]
--           , passthrough_up: matchTreeChangeSort (Var %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> Var %∂.^ [ Ext %∂.^ [ gamma ] ]
--           -- when Zero unwraps via a down change, then sends up a change that
--           -- will cause a Free to wrap 
--           , unwrap_down: matchTreeChangeSort
--               (Var %|∂.^ [ Ext %|∂-^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _))
--               \{ gamma } ->
--                 { up: pure $ Var %∂.^ [ ExtFree %∂+^ [] << (gamma # outerEndpoint # id) >> [] ]
--                 , down: pure $ Var %∂.^ [ gamma ]
--                 }
--           , unwrap_up: const empty
--           , wrap_down: const empty
--           -- matches on an up change sent by the result of unwrapping a Free
--           , wrap_up: matchTreeChangeSort
--               (Var %|∂.^ [ ExtFree %|∂+^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _))
--               \{ gamma } ->
--                 { up: pure $ Var %∂.^ [ gamma ]
--                 , down: empty
--                 }
--           }
--         ]
--     }
--     where
--     _gamma = matchialVar (Proxy :: Proxy "gamma")
--   derivPropagRules Suc = DerivPropagRule
--     { kids: List.fromFoldable
--         [ { passthrough_down: matchTreeChangeSort (Var %|∂.^ [ Ext %|∂.^ [ _gamma ] ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> Var %∂.^ [ gamma ]
--           , passthrough_up: matchTreeChangeSort (Var %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> Var %∂.^ [ Ext %∂.^ [ gamma ] ]
--           , unwrap_down: matchTreeChangeSort
--               (Var %|∂.^ [ Ext %|∂-^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _))
--               \{ gamma } ->
--                 { up: empty
--                 , down: pure $ Var %∂.^ [ gamma ]
--                 }
--           , unwrap_up: const empty
--           , wrap_down: matchTreeChangeSort
--               (Var %|∂.^ [ Ext %|∂+^ [] << _gamma >> [] ] :: Tree (Matchial (gamma :: _) _ _))
--               \{ gamma } ->
--                 { up: empty
--                 , down: pure $ Var %∂.^ [ gamma ]
--                 }
--           , wrap_up: const empty
--           }
--         ]
--     }
--     where
--     _gamma = matchialVar (Proxy :: Proxy "gamma")
--   derivPropagRules Ref = DerivPropagRule
--     { kids: List.fromFoldable
--         [ { passthrough_down: matchTreeChangeSort
--               (Term %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _))
--               \{ gamma } ->
--                 Var %∂.^ [ gamma ]
--           , passthrough_up: matchTreeChangeSort
--               (Var %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _))
--               \{ gamma } ->
--                 Term %∂.^ [ gamma ]
--           , unwrap_down: const empty
--           , unwrap_up: const empty
--           , wrap_down: const empty
--           , wrap_up: const empty
--           }
--         ]
--     }
--     where
--     _gamma = matchialVar (Proxy :: Proxy "gamma")
--   derivPropagRules Lam = DerivPropagRule
--     { kids: List.fromFoldable
--         [ { passthrough_down: matchTreeChangeSort (Term %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> Term %∂.^ [ Ext %∂.^ [ gamma ] ]
--           , passthrough_up: matchTreeChangeSort (Term %|∂.^ [ Ext %|∂.^ [ _gamma ] ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> Term %∂.^ [ gamma ]
--           , unwrap_down: const empty
--           , unwrap_up: const empty
--           , wrap_down: const empty
--           , wrap_up: const empty
--           }
--         ]
--     }
--     where
--     _gamma = matchialVar (Proxy :: Proxy "gamma")
--   derivPropagRules App = DerivPropagRule
--     { kids: List.fromFoldable
--         [ { passthrough_down: matchTreeChangeSort (Term %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> Term %∂.^ [ gamma ]
--           , passthrough_up: matchTreeChangeSort (Term %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> Term %∂.^ [ gamma ]
--           , unwrap_down: const empty
--           , unwrap_up: const empty
--           , wrap_down: const empty
--           , wrap_up: const empty
--           }
--         , { passthrough_down: matchTreeChangeSort (Term %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> Term %∂.^ [ gamma ]
--           , passthrough_up: matchTreeChangeSort (Term %|∂.^ [ _gamma ] :: Tree (Matchial (gamma :: _) _ _)) \{ gamma } -> Term %∂.^ [ gamma ]
--           , unwrap_down: const empty
--           , unwrap_up: const empty
--           , wrap_down: const empty
--           , wrap_up: const empty
--           }
--         ]
--     }
--     where
--     _gamma = matchialVar (Proxy :: Proxy "gamma")
--   derivPropagRules Hole = DerivPropagRule
--     { kids: mempty }

-- instance IsDerivPropagLanguage D S

-- instance HasPropagRules D S where
--   propagRules = mempty

-- instance IsPropagLanguage D S
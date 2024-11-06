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
import Data.Tuple.Nested ((/\))
import Pantograph.Library.DerivePropagationAdjustRulesFromDerRules (propagationAdjustRules)
import Pantograph.Pretty (class Pretty)
import Pantograph.Utility (bug, todo)
import Type.Proxy (Proxy(..))

data S
  = Emp
  | Ext
  | Var
  | Term

derive instance Generic S _

instance SuperLbl S S where
  injectLbl = identity

instance Show S where
  show x = genericShow x

instance PrettyTreeLbl S where
  prettyTree Emp Nil = "∅"
  prettyTree Ext (g : Nil) = "E" <> g
  prettyTree Var (g : Nil) = "Var " <> g
  prettyTree Term (g : Nil) = "Term " <> g
  prettyTree _ _ = bug "invalid S"

instance Eq S where
  eq x = genericEq x

instance Pretty S where
  pretty = show

instance IsSortRuleLbl S

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

instance PrettyTreeLbl D where
  prettyTree Free Nil = "F"
  prettyTree Zero Nil = "Z"
  prettyTree Suc (n : Nil) = "S" <> n
  prettyTree Ref (x : Nil) = "#" <> x
  prettyTree Lam (b : Nil) = "(λ " <> b <> ")"
  prettyTree App (f : a : Nil) = "(" <> f <> " " <> a <> ")"
  prettyTree Hole Nil = "?"
  prettyTree _ _ = bug "invalid D"

instance IsDerRuleLbl D

instance HasDerRules D S where
  derRules = case _ of
    Free -> Var %^ [ Ext %^ [ g ] ] -| []
    Zero -> Var %^ [ Ext %^ [ g ] ] -| [ Var %^ [ g ] ]
    Suc -> Var %^ [ g ] -| [ Var %^ [ Ext %^ [ g ] ] ]
    Ref -> Term %^ [ g ] -| [ Var %^ [ g ] ]
    Lam -> Term %^ [ g ] -| [ Term %^ [ Ext %^ [ g ] ] ]
    App -> Term %^ [ g ] -| [ Term %^ [ g ], Term %^ [ g ] ]
    Hole -> Term %^ [ g ] -| []
    where
    g = mkMetaVar "gamma"

instance IsLanguage D S

instance HasAdjustRules D S where
  adjustRules = modifyAdjustRules <> propagationAdjustRules
    where
    modifyAdjustRules = List.fromFoldable
      [ AdjustRule
          { name: "replace Zero with Free"
          , rule: \_mb_th tm -> tm # matchTree ((Zero // [ g /\ ?a ]) %^ []) # map (todo "")
          }
      ]
    g = ?a

-- instance HasDerAdjustRules D S where
--   derAdjustRules ZeroWeak = DerAdjustRule
--     { kids: mempty }
--   derAdjustRules SucWeak = DerAdjustRule
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
--   derAdjustRules Free = DerAdjustRule
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
--   derAdjustRules Zero = DerAdjustRule
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
--   derAdjustRules Suc = DerAdjustRule
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
--   derAdjustRules Ref = DerAdjustRule
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
--   derAdjustRules Lam = DerAdjustRule
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
--   derAdjustRules App = DerAdjustRule
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
--   derAdjustRules Hole = DerAdjustRule
--     { kids: mempty }

-- instance IsDerAdjustLanguage D S

-- instance HasAdjustRules D S where
--   adjustRules = mempty

-- instance IsAdjustLanguage D S
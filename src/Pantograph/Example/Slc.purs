-- | Scoped lambda calculus
module Pantograph.Example.Slc where

import Pantograph.Grammar
import Prelude

import Control.MonadPlus (empty)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe)
import Data.Set as Set
import Pantograph.RevList (RevList(..))
import Pantograph.Tree (ChangeLabel(..), Tooth(..), Tree(..), (◃))
import Pantograph.Utility (bug, todo, unimplemented)

data S
  = Ctx_S -- Sort
  | Nil_S -- Sort
  | Cons_S -- Sort -> Sort
  | Var_S -- Sort -> Sort
  | Term_S -- Sort -> Sort

data D
  = Zero_D -- Var
  | Suc_D -- Var -> Var
  | Var_D -- Var -> Term
  | Lam_D -- Term -> Term
  | App_D -- Term -> Term -> Term

--------------------------------------------------------------------------------

derivRules :: DerivRules D S

-- Var

derivRules Zero_D =
  mkDerivRule "Zero" []
    []
    ----
    (pure (Inject_SortLabel Var_S) ◃ ((pure (Inject_SortLabel Cons_S) ◃ ((Left gamma ◃ Nil) : Nil)) : Nil))
  where
  gamma = RulialVar "gamma"

derivRules Suc_D =
  mkDerivRule "Suc" []
    [ pure (Inject_SortLabel Var_S) ◃ ((Left gamma ◃ Nil) : Nil) ]
    ----
    (pure (Inject_SortLabel Var_S) ◃ ((pure (Inject_SortLabel Cons_S) ◃ ((Left gamma ◃ Nil) : Nil)) : Nil))
  where
  gamma = RulialVar "gamma"

-- Term

derivRules Var_D =
  mkDerivRule "Var" [ gamma ]
    [ pure (Inject_SortLabel Var_S) ◃ ((Left gamma ◃ Nil) : Nil) ]
    ----
    (pure (Inject_SortLabel Term_S) ◃ ((Left gamma ◃ Nil) : Nil))
  where
  gamma = RulialVar "gamma"

derivRules Lam_D =
  mkDerivRule "Lam" [ gamma ]
    [ pure (Inject_SortLabel Term_S) ◃ ((pure (Inject_SortLabel Cons_S) ◃ ((Left gamma ◃ Nil) : Nil)) : Nil) ]
    ----
    (pure (Inject_SortLabel Term_S) ◃ ((Left gamma ◃ Nil) : Nil))
  where
  gamma = RulialVar "gamma"

derivRules App_D =
  mkDerivRule "App" [ gamma ]
    [ pure (Inject_SortLabel Term_S) ◃ ((Left gamma ◃ Nil) : Nil)
    , pure (Inject_SortLabel Term_S) ◃ ((Left gamma ◃ Nil) : Nil)
    ]
    ----
    (pure (Inject_SortLabel Term_S) ◃ ((Left gamma ◃ Nil) : Nil))
  where
  gamma = RulialVar "gamma"

propagRules :: PropagRules D S
propagRules =
  [ PropagRule "Down Zero" \_ -> case _ of
      -- -- identity change
      -- Boundary Down (Congruence (Right (Inject_SortLabel Ctx_S)) ◃ Nil) ◃ (kid : Nil) -> pure kid
      -- Boundary Down (Plus (Tooth (Right (Inject_SortLabel Ctx_S)) (RevList Nil)) ◃ (kid_ch : Nil)) ◃ (kid : Nil) -> ?A
      -- Boundary Down (Minus _ ◃ yyy) ◃ (kid : Nil) -> ?A
      -- Boundary Down (Replace _ _ ◃ yyy) ◃ (kid : Nil) -> ?A
      -- Boundary Down _ ◃ (_ : Nil) -> bug "invalid Boundary Change"
      -- Boundary Down ch ◃ _ -> bug "invalid PropagDeriv"
      _ -> empty
  ] # List.fromFoldable

canonicalDerivOfSort :: Sort S -> Maybe (Deriv D S)
canonicalDerivOfSort _ = unimplemented "canonicalDerivOfSort"

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

mkTree :: forall a f. Foldable f => a -> f (Tree a) -> Tree a
mkTree a = Tree a <<< List.fromFoldable

mkDerivRule
  :: forall s f1 f2
   . Foldable f1
  => Foldable f2
  => String
  -> f1 RulialVar
  -> f2 (RulialSort s)
  -> RulialSort s
  -> DerivRule s
mkDerivRule label params args sort = DerivRule label (Set.fromFoldable params) (List.fromFoldable args) sort


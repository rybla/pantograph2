-- | Scoped lambda calculus
module Pantograph.Example.Slc where

import Pantograph.Grammar
import Pantograph.Tree
import Prelude

import Control.MonadPlus (empty)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe)
import Pantograph.RevList (RevList(..))
import Pantograph.Utility (unimplemented)

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
  mkDerivRule "Zero"
    []
    ----
    (pure (Inject_SortLabel Var_S) ◃ ((pure (Inject_SortLabel Cons_S) ◃ ((Left gamma ◃ Nil) : Nil)) : Nil))
  where
  gamma = RulialVar "gamma"

derivRules Suc_D =
  mkDerivRule "Suc"
    [ Plus (Tooth (pure (Inject_SortLabel Var_S)) (RevList Nil) Nil) ◃ ((Congruence (Left gamma) ◃ Nil) : Nil) ]
    ----
    (pure (Inject_SortLabel Var_S) ◃ ((pure (Inject_SortLabel Cons_S) ◃ ((Left gamma ◃ Nil) : Nil)) : Nil))
  where
  gamma = RulialVar "gamma"

-- Term

derivRules Var_D =
  mkDerivRule "Var"
    [ Replace (pure (Inject_SortLabel Var_S) ◃ ((Left gamma ◃ Nil) : Nil)) (pure (Inject_SortLabel Term_S) ◃ ((Left gamma ◃ Nil) : Nil)) ◃ Nil ]
    ----
    (pure (Inject_SortLabel Term_S) ◃ ((Left gamma ◃ Nil) : Nil))
  where
  gamma = RulialVar "gamma"

derivRules Lam_D =
  mkDerivRule "Lam"
    [ Minus (Tooth (pure (Inject_SortLabel Var_S)) (RevList Nil) Nil) ◃ ((Congruence (Left gamma) ◃ Nil) : Nil) ]
    ----
    (pure (Inject_SortLabel Term_S) ◃ ((Left gamma ◃ Nil) : Nil))
  where
  gamma = RulialVar "gamma"

derivRules App_D =
  mkDerivRule "App"
    [ Congruence (pure (Inject_SortLabel Term_S)) ◃ ((Congruence (Left gamma) ◃ Nil) : Nil)
    , Congruence (pure (Inject_SortLabel Term_S)) ◃ ((Congruence (Left gamma) ◃ Nil) : Nil)
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
  :: forall s f
   . Foldable f
  => String
  -> f (RulialSortChange s)
  -> RulialSort s
  -> DerivRule s
mkDerivRule label args sort = DerivRule label (List.fromFoldable args) sort


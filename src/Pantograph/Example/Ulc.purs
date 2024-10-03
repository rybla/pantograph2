-- | Untyped lambda calculus
module Pantograph.Example.Ulc where

import Pantograph.Grammar
import Prelude

import Control.Plus (empty)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Set as Set
import Pantograph.Tree (Tree(..), (◃))
import Pantograph.Utility (bug)

data S
  = String_S
  | Term_S

data D
  = String_D String
  | Var_D -- String
  | Lam_D -- String, Term
  | App_D -- Term, Term
  | Hole_D

--------------------------------------------------------------------------------

derivRules :: DerivRules D S
derivRules =
  let
    ruleString =
      mkDerivRule "String" []
        []
        (mkTree (Right (Inject_SortLabel Term_S)) [ mkTree (Right (Inject_SortLabel String_S)) [] ])

    ruleVar =
      mkDerivRule "Var" []
        [ mkTree (Right (Inject_SortLabel String_S)) [] ]
        (mkTree (Right (Inject_SortLabel Term_S)) [])

    ruleLam =
      mkDerivRule "Lam" []
        [ mkTree (Right (Inject_SortLabel String_S)) []
        , mkTree (Right (Inject_SortLabel Term_S)) []
        ]
        (mkTree (Right (Inject_SortLabel Term_S)) [])

    ruleApp =
      mkDerivRule "App" []
        [ mkTree (Right (Inject_SortLabel Term_S)) []
        , mkTree (Right (Inject_SortLabel Term_S)) []
        ]
        (mkTree (Right (Inject_SortLabel Term_S)) [])

    ruleHole =
      mkDerivRule "Hole" []
        []
        (mkTree (Right (Inject_SortLabel Term_S)) [])

  in
    case _ of
      String_D _ -> ruleString
      Var_D -> ruleVar
      Lam_D -> ruleLam
      App_D -> ruleApp
      Hole_D -> ruleHole

propagRules :: PropagRules D S
propagRules =
  [ PropagRule "trivial" \_th pl -> case pl of
      -- simply remove boundary
      Boundary _ _ ◃ (kid : Nil) -> pure kid
      _ -> empty
  ] # List.fromFoldable

canonicalDerivOfSort :: Sort S -> Maybe (Deriv D S)
canonicalDerivOfSort (Inject_SortLabel String_S ◃ Nil) = pure (DerivLabel (String_D "") Map.empty ◃ Nil)
canonicalDerivOfSort (Inject_SortLabel String_S ◃ _) = bug "invalid sort"
canonicalDerivOfSort (Inject_SortLabel Term_S ◃ Nil) = pure (DerivLabel Hole_D Map.empty ◃ Nil)
canonicalDerivOfSort (Inject_SortLabel Term_S ◃ _) = bug "invalid sort"

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


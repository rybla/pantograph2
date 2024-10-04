-- | Untyped lambda calculus
module Pantograph.Example.Ulc where

import Pantograph.Grammar
import Pantograph.Tree
import Prelude

import Control.Plus (empty)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Pantograph.Pretty (class Pretty)
import Pantograph.Utility (bug)

data S
  = String_S
  | Term_S

derive instance Generic S _

instance Show S where
  show x = genericShow x

instance Pretty S where
  pretty String_S = "String"
  pretty Term_S = "Term"

data D
  = String_D String
  | Var_D -- String
  | Lam_D -- String, Term
  | App_D -- Term, Term
  | Hole_D

derive instance Generic D _

instance Show D where
  show x = genericShow x

instance Pretty D where
  pretty (String_D str) = "String " <> show str
  pretty Var_D = "Var"
  pretty Lam_D = "Lam"
  pretty App_D = "App"
  pretty Hole_D = "Hole"

--------------------------------------------------------------------------------

derivRules :: DerivRules D S
derivRules =
  let
    ruleString =
      mkDerivRule "String"
        []
        (Right (Inject_SortLabel Term_S) ◃* [ Right (Inject_SortLabel String_S) ◃* [] ])

    ruleVar =
      mkDerivRule "Var"
        [ Replace (pure (Inject_SortLabel String_S) ◃* []) (pure (Inject_SortLabel Term_S) ◃* []) ◃* [] ]
        (Right (Inject_SortLabel Term_S) ◃* [])

    ruleLam =
      mkDerivRule "Lam"
        [ Replace (pure (Inject_SortLabel String_S) ◃* []) (pure (Inject_SortLabel Term_S) ◃* []) ◃* []
        , Congruence (pure (Inject_SortLabel Term_S)) ◃* []
        ]
        (Right (Inject_SortLabel Term_S) ◃* [])

    ruleApp =
      mkDerivRule "App"
        [ Congruence (pure (Inject_SortLabel Term_S)) ◃* []
        , Congruence (pure (Inject_SortLabel Term_S)) ◃* []
        ]
        (Right (Inject_SortLabel Term_S) ◃* [])

    ruleHole =
      mkDerivRule "Hole"
        []
        (Right (Inject_SortLabel Term_S) ◃* [])

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

infix 0 mkTree as ◃*

mkDerivRule
  :: forall s f
   . Foldable f
  => String
  -> f (RulialSortChange s)
  -> RulialSort s
  -> DerivRule s
mkDerivRule label args sort = DerivRule label (List.fromFoldable args) sort


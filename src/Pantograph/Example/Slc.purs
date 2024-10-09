-- | Scoped lambda calculus
module Pantograph.Example.Slc where

import Pantograph.Grammar
import Pantograph.Tree
import Prelude

import Control.MonadPlus (empty)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Pantograph.EitherF (EitherF(..))
import Pantograph.Library.PropagRules (defaultPropagRules)
import Pantograph.Pretty (class Pretty)
import Pantograph.Utility (todo, unimplemented)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

data S
  = Ctx -- Sort
  | Nil -- Sort
  | Ext -- Sort -> Sort
  | Var -- Sort -> Sort
  | Term -- Sort -> Sort

derive instance Generic S _

instance Show S where
  show x = genericShow x

instance Pretty S where
  pretty Ctx = "Ctx"
  pretty Nil = "Nil"
  pretty Ext = "Ext"
  pretty Var = "Var"
  pretty Term = "Term"

instance Eq S where
  eq x = genericEq x

data D
  = Zero -- Var
  | Suc -- Var -> Var
  | Ref -- Var -> Term
  | Lam -- Term -> Term
  | App -- Term -> Term -> Term
  | Hole -- Term

derive instance Generic D _

instance Show D where
  show x = genericShow x

instance Pretty D where
  pretty Zero = "Zero"
  pretty Suc = "Suc"
  pretty Ref = "Var"
  pretty Lam = "Lam"
  pretty App = "App"
  pretty Hole = "Hole"

instance Eq D where
  eq x = genericEq x

--------------------------------------------------------------------------------
-- semantics
--------------------------------------------------------------------------------

gamma :: forall s. Tree (Rulial s)
gamma = Left (RulialVar "gamma") ▵* []

derivRules :: DerivRules D S

-- Var

derivRules Zero =
  mkDerivRule "Zero"
    []
    ----
    (pure Var ▵* [ pure Ext ▵* [ gamma ] ])

derivRules Suc =
  mkDerivRule "Suc"
    [ (pure Ext ▵< ([] /\ [])) ▵∂+ id gamma ]
    ----
    (pure Var ▵* [ pure Ext ▵* [ gamma ] ])

-- Term

derivRules Ref =
  mkDerivRule "Ref"
    [ (pure Var ▵* [ gamma ]) ▵∂~> (pure Term ▵* [ gamma ]) ]
    ----
    (pure Term ▵* [ gamma ])

derivRules Lam =
  mkDerivRule "Lam"
    [ (pure Ext ▵< ([] /\ [])) ▵∂- id gamma ]
    ----
    (pure Term ▵* [ gamma ])

derivRules App =
  mkDerivRule "App"
    [ pure Term ▵∂. [ id gamma ]
    , pure Term ▵∂. [ id gamma ]
    ]
    ----
    (pure Term ▵* [ gamma ])

derivRules Hole =
  mkDerivRule "Hole"
    []
    ----
    (pure Term ▵* [ gamma ])

propagRules :: PropagRules D S
propagRules = defaultPropagRules derivRules <> customPropagRules
  where
  customPropagRules =
    [
    ] # List.fromFoldable

canonicalDerivOfSort :: Sort S -> Maybe (Deriv D S)
canonicalDerivOfSort _ = unimplemented "canonicalDerivOfSort"

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

mkDerivRule label args sort = DerivRule label (List.fromFoldable args) sort


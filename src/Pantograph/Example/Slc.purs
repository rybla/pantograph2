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
-- smart constructors
--------------------------------------------------------------------------------

-- sorts

nil = SortLabel Nil ▵* []
nilₐ = pure (SortLabel Nil) ▵* []

ext ∷ Tree (SortLabel S) → Tree (SortLabel S)
ext g = SortLabel Ext ▵* [ g ]

extₐ g = pure (SortLabel Ext) ▵* [ g ]
extᵪ g = pure (SortLabel Ext) ▵∂. [ g ]
ext₀ =  SortLabel Ext ▵< ([] /\ [])
extXXX = pure (SortLabel Ext) ▵< ([] /\ [])

var g = SortLabel Var ▵* [ g ]
varₐ g = pure (SortLabel Var) ▵* [ g ]
varᵪ g = pure (SortLabel Var) ▵∂. [ g ]

term g = SortLabel Term ▵* [ g ]
termₐ g = pure (SortLabel Term) ▵* [ g ]
termᵪ g = pure (SortLabel Term) ▵∂. [ g ]

-- derivs

zero gamma = DerivLabel Zero (Map.fromFoldable [ gamma_rv /\ gamma ]) ▵* []
zeroₚ gamma = RightF (DerivLabel Zero (Map.fromFoldable [ gamma_rv /\ gamma ])) ▵* []

suc gamma x = DerivLabel Suc (Map.fromFoldable [ gamma_rv /\ gamma ]) ▵* [ x ]
sucₚ gamma x = RightF (DerivLabel Suc (Map.fromFoldable [ gamma_rv /\ gamma ])) ▵* [ x ]

ref gamma x = DerivLabel Ref (Map.fromFoldable [ gamma_rv /\ gamma ]) ▵* [ x ]
refₚ gamma x = RightF (DerivLabel Ref (Map.fromFoldable [ gamma_rv /\ gamma ])) ▵* [ x ]

lam gamma b = DerivLabel Ref (Map.fromFoldable [ gamma_rv /\ gamma ]) ▵* [ b ]
lamₚ gamma b = RightF (DerivLabel Ref (Map.fromFoldable [ gamma_rv /\ gamma ])) ▵* [ b ]

app gamma f a = DerivLabel Ref (Map.fromFoldable [ gamma_rv /\ gamma ]) ▵* [ f, a ]
appₚ gamma f a = (DerivLabel Ref (Map.fromFoldable [ gamma_rv /\ gamma ])) ▵* [ f, a ]

--------------------------------------------------------------------------------
-- semantics
--------------------------------------------------------------------------------

gamma_rv = RulialVar "gamma"

gamma_rs :: Tree (Rulial (SortLabel S))
gamma_rs = Left gamma_rv ▵* []

derivRules :: DerivRules D S

-- Var

derivRules Zero =
  mkDerivRule "Zero"
    [] ----
    (varₐ (extₐ gamma_rs))

derivRules Suc =
  mkDerivRule "Suc"
    [ extXXX ▵∂+ id gamma_rs
    ] ----
    (varₐ (extₐ gamma_rs))

-- Term

derivRules Ref =
  mkDerivRule "Ref"
    [ varₐ gamma_rs ▵∂~> termₐ gamma_rs
    ] ----
    (termₐ gamma_rs)

derivRules Lam =
  mkDerivRule "Lam"
    [ extXXX  ▵∂- id gamma_rs
    ] ----
    (termₐ gamma_rs)

derivRules App =
  mkDerivRule "App"
    [ termᵪ (id gamma_rs)
    , termᵪ (id gamma_rs)
    ] ----
    (termₐ gamma_rs)

derivRules Hole =
  mkDerivRule "Hole"
    [] ----
    (termₐ gamma_rs)

propagRules :: PropagRules D S
propagRules = defaultPropagRules derivRules <> customPropagRules
  where
  customPropagRules =
    [] # List.fromFoldable

canonicalDerivOfSort :: Tree (SortLabel S) -> Maybe (Tree (DerivLabel D S))
canonicalDerivOfSort _ = unimplemented "canonicalDerivOfSort"

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

mkDerivRule label args sort = DerivRule label (List.fromFoldable args) sort


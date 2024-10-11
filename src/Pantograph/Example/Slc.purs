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
import Data.List ((:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Pantograph.EitherF (EitherF(..))
import Pantograph.Library.PropagRules (defaultPropagRules)
import Pantograph.Pretty (class Pretty, parens)
import Pantograph.Utility (bug, todo, unimplemented)

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

instance PrettyTreeLabel S where
  prettyTree Ctx List.Nil = "Ctx"
  prettyTree Nil List.Nil = "Nil"
  prettyTree Ext (gamma : List.Nil) = parens $ "Ext " <> gamma
  prettyTree Var (gamma : List.Nil) = parens $ "Var " <> gamma
  prettyTree Term (gamma : List.Nil) = parens $ "Term " <> gamma
  prettyTree _ _ = bug "invaliid `Tree S`"

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

instance PrettyTreeLabel D where
  prettyTree Zero List.Nil = "Z"
  prettyTree Suc (x : List.Nil) = "S" <> x
  prettyTree Ref (x : List.Nil) = "#" <> x
  prettyTree Lam (b : List.Nil) = parens $ "λ " <> b
  prettyTree App (f : a : List.Nil) = parens $ f <> " " <> a
  prettyTree Hole List.Nil = "??"
  prettyTree _ _ = bug "invalid `Tree D`"

instance Eq D where
  eq x = genericEq x

--------------------------------------------------------------------------------
-- smart constructors
--------------------------------------------------------------------------------

-- sorts

nil = SortLabel Nil %* []
nil_a = pure (SortLabel Nil) %* []

ext g = SortLabel Ext %* [ g ]
ext_a g = pure (SortLabel Ext) %* [ g ]
ext_c g = SortLabel Ext %∂. [ g ]
ext_c' g = pure (SortLabel Ext) %∂. [ g ]
ext_0 = SortLabel Ext %< ([] /\ [])
ext_0_a = pure (SortLabel Ext) %< ([] /\ [])

var g = SortLabel Var %* [ g ]
var_a g = pure (SortLabel Var) %* [ g ]
var_c g = SortLabel Var %∂. [ g ]
var_c' g = pure (SortLabel Var) %∂. [ g ]

term g = SortLabel Term %* [ g ]
term_a g = pure (SortLabel Term) %* [ g ]
term_c g = SortLabel Term %∂. [ g ]
term_c' g = pure (SortLabel Term) %∂. [ g ]

-- derivs

zero gamma = DerivLabel Zero (Map.fromFoldable [ gamma_rv /\ gamma ]) %* []
zero_p gamma = RightF (DerivLabel Zero (Map.fromFoldable [ gamma_rv /\ gamma ])) %* []

suc gamma x = DerivLabel Suc (Map.fromFoldable [ gamma_rv /\ gamma ]) %* [ x ]
suc_p gamma x = RightF (DerivLabel Suc (Map.fromFoldable [ gamma_rv /\ gamma ])) %* [ x ]

ref gamma x = DerivLabel Ref (Map.fromFoldable [ gamma_rv /\ gamma ]) %* [ x ]
ref_p gamma x = RightF (DerivLabel Ref (Map.fromFoldable [ gamma_rv /\ gamma ])) %* [ x ]

lam gamma b = DerivLabel Lam (Map.fromFoldable [ gamma_rv /\ gamma ]) %* [ b ]
lam_p gamma b = RightF (DerivLabel Lam (Map.fromFoldable [ gamma_rv /\ gamma ])) %* [ b ]

app gamma f a = DerivLabel App (Map.fromFoldable [ gamma_rv /\ gamma ]) %* [ f, a ]
app_p gamma f a = (DerivLabel App (Map.fromFoldable [ gamma_rv /\ gamma ])) %* [ f, a ]

--------------------------------------------------------------------------------
-- semantics
--------------------------------------------------------------------------------

gamma_rv = RulialVar "gamma"

gamma_rs :: Tree (Rulial (SortLabel S))
gamma_rs = Left gamma_rv %* []

derivRules :: DerivRules D S

-- Var

derivRules Zero =
  mkDerivRule "Zero"
    [] ----
    (var_a (ext_a gamma_rs))

derivRules Suc =
  mkDerivRule "Suc"
    [ ext_0_a %∂+ id gamma_rs
    ] ----
    (var_a (ext_a gamma_rs))

-- Term

derivRules Ref =
  mkDerivRule "Ref"
    [ var_a gamma_rs %∂~> term_a gamma_rs
    ] ----
    (term_a gamma_rs)

derivRules Lam =
  mkDerivRule "Lam"
    [ term_c' (ext_0_a %∂- id gamma_rs)
    ] ----
    (term_a gamma_rs)

derivRules App =
  mkDerivRule "App"
    [ term_c' (id gamma_rs)
    , term_c' (id gamma_rs)
    ] ----
    (term_a gamma_rs)

derivRules Hole =
  mkDerivRule "Hole"
    [] ----
    (term_a gamma_rs)

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


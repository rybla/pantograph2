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
import Pantograph.Pretty (class Pretty)
import Pantograph.Utility (unimplemented)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

data S
  = Ctx_S -- Sort
  | Nil_S -- Sort
  | Ext_S -- Sort -> Sort
  | Var_S -- Sort -> Sort
  | Term_S -- Sort -> Sort

derive instance Generic S _

instance Show S where
  show x = genericShow x

instance Pretty S where
  pretty Ctx_S = "Ctx"
  pretty Nil_S = "Nil"
  pretty Ext_S = "Ext"
  pretty Var_S = "Var"
  pretty Term_S = "Term"

instance Eq S where
  eq x = genericEq x

data D
  = Zero_D -- Var
  | Suc_D -- Var -> Var
  | Ref_D -- Var -> Term
  | Lam_D -- Term -> Term
  | App_D -- Term -> Term -> Term
  | Hole_D -- Term

derive instance Generic D _

instance Show D where
  show x = genericShow x

instance Pretty D where
  pretty Zero_D = "Zero"
  pretty Suc_D = "Suc"
  pretty Ref_D = "Var"
  pretty Lam_D = "Lam"
  pretty App_D = "App"
  pretty Hole_D = "Hole"

instance Eq D where
  eq x = genericEq x

--------------------------------------------------------------------------------
-- smart exttructors
--------------------------------------------------------------------------------

-- sorts

ctx = pure Ctx_S ▵* []

nil = pure Nil_S ▵* []

ext gamma = pure Ext_S ▵* [ gamma ]

ext_0 = pure Ext_S ▵< ([] /\ [])

var gamma = pure Var_S ▵* [ gamma ]

var' gamma = pure Var_S ▵∂. [ gamma ]

term gamma = pure Term_S ▵* [ gamma ]

term' gamma = pure Term_S ▵∂. [ gamma ]

-- derivs

zero :: MetaSort S -> Deriv D S
zero gamma = DerivLabel Zero_D (Map.fromFoldable [ gamma_rv /\ gamma ]) ▵* []

suc :: MetaSort S -> Deriv D S -> Deriv D S
suc gamma x = DerivLabel Suc_D (Map.fromFoldable [ gamma_rv /\ gamma ]) ▵* [ x ]

zero' :: MetaSort S -> PropagDeriv D S
zero' gamma = RightF (DerivLabel Zero_D (Map.fromFoldable [ gamma_rv /\ gamma ])) ▵* []

suc' :: MetaSort S -> PropagDeriv D S -> PropagDeriv D S
suc' gamma x = RightF (DerivLabel Suc_D (Map.fromFoldable [ gamma_rv /\ gamma ])) ▵* [ x ]

ref :: MetaSort S -> Deriv D S -> Deriv D S
ref gamma x = DerivLabel Ref_D (Map.fromFoldable [ gamma_rv /\ gamma ]) ▵* [ x ]

ref' :: MetaSort S -> PropagDeriv D S -> PropagDeriv D S
ref' gamma x = RightF (DerivLabel Ref_D (Map.fromFoldable [ gamma_rv /\ gamma ])) ▵* [ x ]

lam :: MetaSort S -> Deriv D S -> Deriv D S
lam gamma b = DerivLabel Lam_D (Map.fromFoldable [ gamma_rv /\ gamma ]) ▵* [ b ]

lam' :: MetaSort S -> PropagDeriv D S -> PropagDeriv D S
lam' gamma b = RightF (DerivLabel Lam_D (Map.fromFoldable [ gamma_rv /\ gamma ])) ▵* [ b ]

app :: MetaSort S -> Deriv D S -> Deriv D S -> Deriv D S
app gamma f a = DerivLabel App_D (Map.fromFoldable [ gamma_rv /\ gamma ]) ▵* [ f, a ]

app' :: MetaSort S -> PropagDeriv D S -> PropagDeriv D S -> PropagDeriv D S
app' gamma f a = RightF (DerivLabel (App_D) (Map.fromFoldable [ gamma_rv /\ gamma ])) ▵* [ f, a ]

hole :: MetaSort S -> Deriv D S
hole gamma = DerivLabel Hole_D (Map.fromFoldable [ gamma_rv /\ gamma ]) ▵* []

hole' :: MetaSort S -> PropagDeriv D S
hole' gamma = RightF (DerivLabel Hole_D (Map.fromFoldable [ gamma_rv /\ gamma ])) ▵* []

--------------------------------------------------------------------------------
-- semantics
--------------------------------------------------------------------------------

gamma_rv = RulialVar "gamma" :: RulialVar

rv :: RulialVar -> RulialSort S
rv x = Left x ▵* []

rv' :: RulialVar -> RulialSortChange S
rv' = rv >>> id

derivRules :: DerivRules D S

-- Var

derivRules Zero_D =
  mkDerivRule "Zero"
    []
    ----
    (var (ext (rv gamma_rv)))

derivRules Suc_D =
  mkDerivRule "Suc"
    [ var' (ext_0 ▵∂+ rv' gamma_rv) ]
    ----
    (var (ext (rv gamma_rv)))

-- Term

derivRules Ref_D =
  mkDerivRule "Ref"
    [ var (rv gamma_rv) ▵∂~> term (rv gamma_rv) ]
    ----
    (term (rv gamma_rv))

derivRules Lam_D =
  mkDerivRule "Lam"
    [ ext_0 ▵∂- rv' gamma_rv ]
    ----
    (term (rv gamma_rv))

derivRules App_D =
  mkDerivRule "App"
    [ term' (rv' gamma_rv)
    , term' (rv' gamma_rv)
    ]
    ----
    (term (rv gamma_rv))

derivRules Hole_D =
  mkDerivRule "Hole"
    []
    ----
    (term (rv gamma_rv))

propagRules :: PropagRules D S
propagRules =
  [ PropagRule "Down Zero" \_ -> case _ of
      -- -- identity change
      -- PropagBoundary Down (Congruence (Right (Ctx_S)) ▵ Nil) ▵ (kid : Nil) -> pure kid
      -- PropagBoundary Down (Plus (Tooth (Right (Ctx_S)) (RevList Nil)) ▵ (kid_ch : Nil)) ▵ (kid : Nil) -> ?A
      -- PropagBoundary Down (Minus _ ▵ yyy) ▵ (kid : Nil) -> ?A
      -- PropagBoundary Down (Replace _ _ ▵ yyy) ▵ (kid : Nil) -> ?A
      -- PropagBoundary Down _ ▵ (_ : Nil) -> bug "invalid PropagBoundary Change"
      -- PropagBoundary Down ch ▵ _ -> bug "invalid PropagDeriv"
      _ -> empty
  ] # List.fromFoldable

canonicalDerivOfSort :: Sort S -> Maybe (Deriv D S)
canonicalDerivOfSort _ = unimplemented "canonicalDerivOfSort"

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

mkDerivRule
  :: forall s f
   . Foldable f
  => String
  -> f (RulialSortChange s)
  -> RulialSort s
  -> DerivRule s
mkDerivRule label args sort = DerivRule label (List.fromFoldable args) sort


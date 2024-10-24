module Pantograph.Language where

import Pantograph.Tree
import Prelude

import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Pantograph.EitherF (EitherF(..))
import Pantograph.Pretty (class Pretty, parens, pretty)
import Pantograph.Utility (bug)

--------------------------------------------------------------------------------
-- RulialVar
--------------------------------------------------------------------------------

data RulialVar = RulialVar String

type Rulial = Either RulialVar

type RulialVarSubst = Map RulialVar

--------------------------------------------------------------------------------
-- SortLabel
--------------------------------------------------------------------------------

class (Show s, Eq s, PrettyTreeLabel s) <= IsSortRuleLabel s

data SortLabel s = SortLabel s

--------------------------------------------------------------------------------
-- DerivLabel
--------------------------------------------------------------------------------

class (Show d, Eq d, PrettyTreeLabel d) <= IsDerivRuleLabel d

type DerivLabel d s = DerivLabel' d (SortLabel s)

data DerivLabel' d s
  = DerivLabel d (RulialVarSubst (Tree s))
  | DerivBoundary (Tree (ChangeLabel s))

--------------------------------------------------------------------------------
-- DerivRule
--------------------------------------------------------------------------------

newtype DerivRule s = DerivRule
  { name :: String
  , sort :: Tree (Rulial (SortLabel s))
  , kids ::
      List
        { sort :: Tree (Rulial (SortLabel s))
        , passthrough_down :: Tree (ChangeLabel (SortLabel s)) -> Maybe (Tree (ChangeLabel (SortLabel s)))
        , passthrough_up :: Tree (ChangeLabel (SortLabel s)) -> Maybe (Tree (ChangeLabel (SortLabel s)))
        , wrap_down :: Tree (ChangeLabel (SortLabel s)) -> Maybe { up :: Tree (ChangeLabel (SortLabel s)), down :: Tree (ChangeLabel (SortLabel s)) }
        , wrap_up :: Tree (ChangeLabel (SortLabel s)) -> Maybe { up :: Tree (ChangeLabel (SortLabel s)), down :: Tree (ChangeLabel (SortLabel s)) }
        , unwrap_down :: Tree (ChangeLabel (SortLabel s)) -> Maybe { up :: Tree (ChangeLabel (SortLabel s)), down :: Tree (ChangeLabel (SortLabel s)) }
        , unwrap_up :: Tree (ChangeLabel (SortLabel s)) -> Maybe { up :: Tree (ChangeLabel (SortLabel s)), down :: Tree (ChangeLabel (SortLabel s)) }
        }
  }

type DerivRules d s = d -> DerivRule s

--------------------------------------------------------------------------------
-- PropagDerivLabel
-- TODO: eventually I'll have to deal with the cursor position being somewhere in the PropagDeriv
--------------------------------------------------------------------------------

type PropagDerivLabel d s = PropagDerivLabel' (DerivLabel' d) (SortLabel s)

type PropagDerivLabel' = EitherF PropagDerivLabel''

data PropagDerivLabel'' s = PropagBoundary PropagBoundaryDirection (Tree (ChangeLabel s))

data PropagBoundaryDirection = Up | Down

downPropagBoundary :: forall d s. Tree (ChangeLabel s) -> Tree (PropagDerivLabel' d s) -> Tree (PropagDerivLabel' d s)
downPropagBoundary ch kid = LeftF (PropagBoundary Down ch) %* [ kid ]

upPropagBoundary :: forall d s. Tree (ChangeLabel s) -> Tree (PropagDerivLabel' d s) -> Tree (PropagDerivLabel' d s)
upPropagBoundary ch kid = LeftF (PropagBoundary Up ch) %* [ kid ]

infix 1 downPropagBoundary as ↓
infix 1 upPropagBoundary as ↑

--------------------------------------------------------------------------------
-- PropagRule
--------------------------------------------------------------------------------

newtype PropagRule d s = PropagRule
  { name :: String
  , rule :: Maybe (Tooth (PropagDerivLabel d s)) -> Tree (PropagDerivLabel d s) -> Maybe (Tree (PropagDerivLabel d s))
  }

type PropagRules d s = List (PropagRule d s)

--------------------------------------------------------------------------------
-- instances
--------------------------------------------------------------------------------

derive instance Generic RulialVar _

instance Show RulialVar where
  show x = genericShow x

instance Pretty RulialVar where
  pretty (RulialVar str) = "$" <> str

instance PrettyTreeLabel RulialVar where
  prettyTree rv Nil = pretty rv
  prettyTree _ _ = bug "invalid `Tree RulialVar`"

instance Eq RulialVar where
  eq x = genericEq x

instance Ord RulialVar where
  compare x = genericCompare x

derive instance Generic (SortLabel s) _

instance Pretty s => Pretty (SortLabel s) where
  pretty (SortLabel t) = pretty t

instance PrettyTreeLabel s => PrettyTreeLabel (SortLabel s) where
  prettyTree (SortLabel s) = prettyTree s

instance Show s => Show (SortLabel s) where
  show x = genericShow x

instance Eq s => Eq (SortLabel s) where
  eq x = genericEq x

derive instance Functor SortLabel
derive instance Foldable SortLabel
derive instance Traversable SortLabel

derive instance Generic (DerivLabel' d s) _

instance (Show s, Show d) => Show (DerivLabel' d s) where
  show x = genericShow x

instance (PrettyTreeLabel s, Pretty d) => Pretty (DerivLabel' d s) where
  pretty (DerivLabel d sigma) = parens $ pretty d <> " " <> pretty sigma
  pretty (DerivBoundary ch) = parens $ "!! " <> pretty ch

instance (PrettyTreeLabel s, PrettyTreeLabel d) => PrettyTreeLabel (DerivLabel' d s) where
  prettyTree (DerivLabel d _sigma) kids = prettyTree d kids
  prettyTree (DerivBoundary ch) (kid : Nil) = parens $ pretty ch <> " !! " <> kid
  prettyTree _ _ = bug "invalid `Tree (DerivLabel' d s)`"

instance (Eq s, Eq d) => Eq (DerivLabel' d s) where
  eq x = genericEq x

derive instance Functor (DerivLabel' d)

derive instance Generic (DerivRule s) _

derive instance Newtype (DerivRule s) _

derive instance Generic (PropagDerivLabel'' s) _

instance Show s => Show (PropagDerivLabel'' s) where
  show x = genericShow x

instance PrettyTreeLabel s => Pretty (PropagDerivLabel'' s) where
  pretty (PropagBoundary dir ch) = parens $ "!! " <> pretty dir <> " " <> pretty ch

instance PrettyTreeLabel s => PrettyTreeLabel (PropagDerivLabel'' s) where
  prettyTree (PropagBoundary dir ch) (kid : Nil) = parens $ pretty ch <> " " <> pretty dir <> "  " <> kid
  prettyTree _ _ = bug "invalid `PropagDerivLabel' s`"

instance Eq s => Eq (PropagDerivLabel'' s) where
  eq x = genericEq x

derive instance Functor PropagDerivLabel''

derive instance Generic PropagBoundaryDirection _

instance Show PropagBoundaryDirection where
  show x = genericShow x

instance Pretty PropagBoundaryDirection where
  pretty Up = "↑"
  pretty Down = "↓"

instance Eq PropagBoundaryDirection where
  eq x = genericEq x

derive instance Generic (PropagRule d s) _

derive instance Newtype (PropagRule d s) _


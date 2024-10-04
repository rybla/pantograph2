-- | Grammar
-- |
-- | conventions:
-- |   - type variable `s` corresponds to type of specific sort labels
-- |   - type variable `d` corresponds to type of specific derivation rule labels
module Pantograph.Grammar where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Foldable (fold, intercalate, length)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe')
import Data.Ord.Generic (genericCompare)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Pantograph.Pretty (class Pretty, pretty)
import Pantograph.Tree (Change, ChangeLabel, Path, Tooth, Tree(..))
import Pantograph.Utility (bug)

--------------------------------------------------------------------------------
-- RulialVar
--------------------------------------------------------------------------------

data RulialVar = RulialVar String

derive instance Generic RulialVar _

instance Show RulialVar where
  show x = genericShow x

instance Pretty RulialVar where
  pretty (RulialVar str) = "$" <> str

instance Eq RulialVar where
  eq x = genericEq x

instance Ord RulialVar where
  compare x = genericCompare x

type Rulial = Either RulialVar

type RulialVarSubst = Map RulialVar

applyRulialVarSubst :: forall a. RulialVarSubst (Tree a) -> Tree (Rulial a) -> Tree a
applyRulialVarSubst sigma (Tree (Left rv) _) = sigma # Map.lookup rv # fromMaybe' \_ -> bug "a RulialSort used a RuilialVar that wasn't substituted by the RulialVarSubst"
applyRulialVarSubst sigma (Tree (Right sl) kids) = Tree sl (kids <#> applyRulialVarSubst sigma)

--------------------------------------------------------------------------------
-- MetaVar
--------------------------------------------------------------------------------

data MetaVar = MetaVar String

derive instance Generic MetaVar _

instance Show MetaVar where
  show x = genericShow x

instance Pretty MetaVar where
  pretty (MetaVar str) = "?" <> str

instance Eq MetaVar where
  eq x = genericEq x

instance Ord MetaVar where
  compare x = genericCompare x

type Meta = Either MetaVar

type MetaVarSubst = Map MetaVar

applyMetaVarSubst :: forall a. MetaVarSubst (Tree a) -> Tree (Meta a) -> Tree a
applyMetaVarSubst sigma (Tree (Left rv) _) = sigma # Map.lookup rv # fromMaybe' \_ -> bug "a MetaSort used a RuilialVar that wasn't substituted by the MetaVarSubst"
applyMetaVarSubst sigma (Tree (Right sl) kids) = Tree sl (kids <#> applyMetaVarSubst sigma)

--------------------------------------------------------------------------------
-- Sort
--------------------------------------------------------------------------------

data SortLabel s = Inject_SortLabel s

derive instance Generic (SortLabel s) _

instance Show s => Show (SortLabel s) where
  show x = genericShow x

instance Pretty s => Pretty (SortLabel s) where
  pretty (Inject_SortLabel s) = pretty s

instance Eq s => Eq (SortLabel s) where
  eq x = genericEq x

derive instance Functor SortLabel

type Sort s = Tree (SortLabel s)
type SortChange s = Change (SortLabel s)

type MetaSort s = Tree (Meta (SortLabel s))
type MetaSortChange s = Change (Meta (SortLabel s))

type RulialSort s = Tree (Rulial (SortLabel s))
type RulialSortChange s = Change (Rulial (SortLabel s))

--------------------------------------------------------------------------------
-- Deriv
--------------------------------------------------------------------------------

type DerivLabel d s = DerivLabel' d s
data DerivLabel' d s = DerivLabel d (RulialVarSubst (Tree s))

derive instance Generic (DerivLabel' d s) _

instance (Show s, Show d) => Show (DerivLabel' d s) where
  show x = genericShow x

instance (Pretty s, Pretty d) => Pretty (DerivLabel' d s) where
  pretty (DerivLabel d sigma) = pretty d <> " | " <> pretty sigma

instance (Eq s, Eq d) => Eq (DerivLabel' d s) where
  eq x = genericEq x

derive instance Functor (DerivLabel' d)

type Deriv d s = Tree (DerivLabel d s)
type DerivChange d s = Change (DerivLabel d s)

--------------------------------------------------------------------------------
-- DerivRule
--------------------------------------------------------------------------------

type DerivRule s = DerivRule' (Rulial (SortLabel s))
data DerivRule' s =
  DerivRule
    String -- name
    (List (Change s)) -- for each kid, sort change oriented from kid to parent
    (Tree s) -- parent sort

derive instance Generic (DerivRule' s) _

instance Show s => Show (DerivRule s) where
  show x = genericShow x

instance Pretty s => Pretty (DerivRule s) where
  pretty (DerivRule name kids sort) =
    [ [ "DerivRule " <> name <> "\n" ]
    , if (kids # length) == 0 then [] else kids # map (pretty >>> ("  " <> _)) # Array.fromFoldable
    , [ "  ---------------------------------------------" ]
    , [ "  " <> (sort # pretty) ]
    ]
      # fold >>> intercalate "\n"

instance Eq s => Eq (DerivRule s) where
  eq x = genericEq x

derive instance Functor DerivRule'

type DerivRules d s = d -> DerivRule s

--------------------------------------------------------------------------------
-- PropagDeriv
--------------------------------------------------------------------------------

type PropagDerivLabel d s = PropagDerivLabel' d (Meta (SortLabel s))
data PropagDerivLabel' d s
  = Inject_PropagDerivLabel (DerivLabel' d s)
  | Boundary BoundaryDirection (Tree (ChangeLabel s))

derive instance Generic (PropagDerivLabel' d s) _

instance (Show s, Show d) => Show (PropagDerivLabel' d s) where
  show x = genericShow x

instance (Pretty s, Pretty d) => Pretty (PropagDerivLabel' d s) where
  pretty (Inject_PropagDerivLabel dl) = pretty dl
  pretty (Boundary dir ch) = pretty dir <> " " <> pretty ch

instance (Eq s, Eq d) => Eq (PropagDerivLabel' d s) where
  eq x = genericEq x

derive instance Functor (PropagDerivLabel' d)

data BoundaryDirection
  = Up
  | Down

derive instance Generic BoundaryDirection _

instance Show BoundaryDirection where
  show x = genericShow x

instance Pretty BoundaryDirection where
  pretty Up = "↑"
  pretty Down = "↓"

instance Eq BoundaryDirection where
  eq x = genericEq x

type PropagDeriv d s = Tree (PropagDerivLabel d s)
type PropagDerivTooth d s = Tooth (PropagDerivLabel d s)
type PropagDerivPath d s = Path (PropagDerivLabel d s)

--------------------------------------------------------------------------------
-- PropagDeriv
--------------------------------------------------------------------------------

data PropagRule d s = PropagRule String (Maybe (PropagDerivTooth d s) -> PropagDeriv d s -> Maybe (PropagDeriv d s))

type PropagRules d s = List (PropagRule d s)


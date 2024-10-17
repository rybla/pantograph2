-- | Grammar
-- |
-- | conventions:
-- |   - type variable `d` corresponds to type of specific derivation rule labels
-- |   - type variable `s` corresponds to type of specific sort labels
module Pantograph.Grammar where

import Pantograph.Tree
import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, fold, intercalate, length)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe')
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

type Rulial = Either RulialVar

type RulialVarSubst = Map RulialVar

applyRulialVarSubstToTree :: forall s. RulialVarSubst (Tree s) -> Tree (Rulial s) -> Tree s
applyRulialVarSubstToTree sigma (Tree (Left x) _) = sigma # Map.lookup x # fromMaybe' \_ -> bug "a RulialSort used a RuilialVar that wasn't substituted by the RulialVarSubst"
applyRulialVarSubstToTree sigma (Tree (Right sl) kids) = Tree sl (kids <#> applyRulialVarSubstToTree sigma)

applyRulialVarSubstToChange :: forall s. RulialVarSubst (Tree s) -> Tree (ChangeLabel (Rulial s)) -> Tree (ChangeLabel s)
applyRulialVarSubstToChange sigma (Congruence (Left x) % Nil) = sigma # Map.lookup x # fromMaybe' (\_ -> bug "RulialVar was not substituted") # id
applyRulialVarSubstToChange sigma (Congruence (Right s) % kids) = Congruence s % (kids # map (applyRulialVarSubstToChange sigma))
applyRulialVarSubstToChange sigma (Plus th % (kid : Nil)) = Plus (th # applyRulialVarSubstToTooth sigma) % ((kid # applyRulialVarSubstToChange sigma) : Nil)
applyRulialVarSubstToChange sigma (Minus th % (kid : Nil)) = Minus (th # applyRulialVarSubstToTooth sigma) % ((kid # applyRulialVarSubstToChange sigma) : Nil)
applyRulialVarSubstToChange sigma (Replace t t' % Nil) = Replace (t # applyRulialVarSubstToTree sigma) (t' # applyRulialVarSubstToTree sigma) % Nil
applyRulialVarSubstToChange _sigma _ = bug "invalid Change"

applyRulialVarSubstToTooth :: forall s. RulialVarSubst (Tree s) -> Tooth (Rulial s) -> Tooth s
applyRulialVarSubstToTooth sigma (Tooth (Right s) l r) = Tooth s (l # map (applyRulialVarSubstToTree sigma)) (r # map (applyRulialVarSubstToTree sigma))
applyRulialVarSubstToTooth sigma _ = bug "can't have rulialVar at Tooth"

--------------------------------------------------------------------------------
-- Sort
--------------------------------------------------------------------------------

data SortLabel s = SortLabel s

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

--------------------------------------------------------------------------------
-- Deriv
--------------------------------------------------------------------------------

type DerivLabel d s = DerivLabel' d (SortLabel s)

data DerivLabel' d s
  = DerivLabel d (RulialVarSubst (Tree s))
  | DerivBoundary (Tree (ChangeLabel s))

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

--------------------------------------------------------------------------------
-- DerivRule
--------------------------------------------------------------------------------

-- TODO: need to _split_ kid changes into two changes -- an "input" change and
-- an "output" change that compose to the entire change between the kid and
-- parent. the "input" change will be matched against in the propagation rules,
-- to see if the propagation rule applies. the "output" change will be result in
-- the resulting change put on a kid/parent going in some direction. there might be an aspect 
newtype DerivRule s = DerivRule
  { sort :: Tree (Rulial (SortLabel s))
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

derive instance Generic (DerivRule s) _

derive instance Newtype (DerivRule s) _

-- instance Show s => Show (DerivRule s) where
--   show x = genericShow x

-- instance PrettyTreeLabel s => Pretty (DerivRule s) where
--   pretty (DerivRule name kids sort) =
--     [ [ "(DerivRule " <> name ]
--     , if (kids # length) == 0 then [] else kids # map (pretty >>> ("  " <> _)) # Array.fromFoldable
--     , [ "  ---------------------------------------------" ]
--     , [ "  " <> (sort # pretty) ]
--     , [ ")" ]
--     ]
--       # fold >>> intercalate "\n"

-- instance Eq s => Eq (DerivRule s) where
--   eq x = genericEq x

-- derive instance Functor DerivRule

type DerivRules d s = d -> DerivRule s

-- getParentSortOfDerivTree :: forall d s. DerivRules d s -> Tree (DerivLabel d s) -> Tree (SortLabel s)
-- getParentSortOfDerivTree derivRules (dl % _) = getParentSortOfDerivLabel derivRules dl

-- getParentSortOfDerivLabel :: forall d s. DerivRules d s -> DerivLabel d s -> Tree (SortLabel s)
-- getParentSortOfDerivLabel derivRules (DerivLabel d sigma) = parentSort # applyRulialVarSubstToTree sigma
--   where
--   DerivRule _name _kidChanges parentSort = derivRules d
-- getParentSortOfDerivLabel _ (DerivBoundary ch) = outerEndpoint ch

-- getKidSortChangesOfDerivLabel :: forall d s. DerivRules d s -> DerivLabel d s -> List (Tree (ChangeLabel (SortLabel s)))
-- getKidSortChangesOfDerivLabel derivRules (DerivLabel d sigma) = kidChanges # map (applyRulialVarSubstToChange sigma)
--   where
--   DerivRule _name kidChanges _parentSort = derivRules d
-- getKidSortChangesOfDerivLabel _ (DerivBoundary ch) = ch : Nil

-- getKidSortsOfDerivLabel :: forall d s. DerivRules d s -> DerivLabel d s -> List (Tree (SortLabel s))
-- getKidSortsOfDerivLabel derivRules dl = getKidSortChangesOfDerivLabel derivRules dl # map innerEndpoint

--------------------------------------------------------------------------------
-- Insertion
--------------------------------------------------------------------------------

data Insertion d s =
  Insertion
    String -- name
    (Path (DerivLabel d s)) -- path to insert
    (Tree (ChangeLabel (SortLabel s))) -- outward change at outside of insert
    (Tree (ChangeLabel (SortLabel s))) -- inward  change at inside of insert

--------------------------------------------------------------------------------
-- PropagDeriv
-- TODO: eventually I'll have to deal with the cursor position being somewhere in the PropagDeriv
--------------------------------------------------------------------------------

-- data PropagDerivLabel d s
--   = Inject_PropagDerivLabel (DerivLabel d s)
--   | PropagBoundary PropagBoundaryDirection (Tree (ChangeLabel s))

-- derive instance Generic (PropagDerivLabel d s) _

-- instance (Show d, Show s) => Show (PropagDerivLabel d s) where
--   show x = genericShow x

-- instance (Pretty d, Pretty s) => Pretty (PropagDerivLabel d s) where
--   pretty (Inject_PropagDerivLabel dl) = pretty dl
--   pretty (PropagBoundary dir ch) = pretty dir <> " " <> pretty ch

-- instance (Eq d, Eq s) => Eq (PropagDerivLabel d s) where
--   eq x = genericEq x

-- derive instance Functor (PropagDerivLabel d)

type PropagDerivLabel d s = EitherF PropagDerivLabel' (DerivLabel' d) (SortLabel s)

data PropagDerivLabel' s = PropagBoundary PropagBoundaryDirection (Tree (ChangeLabel s))

-- getParentSortOfPropagDerivTree :: forall d s. DerivRules d s -> Tree (PropagDerivLabel d s) -> Tree (SortLabel s)
-- getParentSortOfPropagDerivTree derivRules (pl % _) = getParentSortOfPropagDerivLabel derivRules pl

-- getParentSortOfPropagDerivLabel :: forall d s. DerivRules d s -> PropagDerivLabel d s -> Tree (SortLabel s)
-- getParentSortOfPropagDerivLabel _derivRules (LeftF (PropagBoundary _dir ch)) = outerEndpoint ch
-- getParentSortOfPropagDerivLabel derivRules (RightF dl) = getParentSortOfDerivLabel derivRules dl

derive instance Generic (PropagDerivLabel' s) _

instance Show s => Show (PropagDerivLabel' s) where
  show x = genericShow x

instance PrettyTreeLabel s => Pretty (PropagDerivLabel' s) where
  pretty (PropagBoundary dir ch) = parens $ "!! " <> pretty dir <> " " <> pretty ch

instance PrettyTreeLabel s => PrettyTreeLabel (PropagDerivLabel' s) where
  prettyTree (PropagBoundary dir ch) (kid : Nil) = parens $ pretty ch <> " " <> pretty dir <> "  " <> kid
  prettyTree _ _ = bug "invalid `PropagDerivLabel' s`"

instance Eq s => Eq (PropagDerivLabel' s) where
  eq x = genericEq x

derive instance Functor PropagDerivLabel'

data PropagBoundaryDirection
  = Up
  | Down

derive instance Generic PropagBoundaryDirection _

instance Show PropagBoundaryDirection where
  show x = genericShow x

instance Pretty PropagBoundaryDirection where
  pretty Up = "↑"
  pretty Down = "↓"

instance Eq PropagBoundaryDirection where
  eq x = genericEq x

-- type PropagDeriv d s = Tree (PropagDerivLabel d s)
-- type PropagDerivTooth d s = Tooth (PropagDerivLabel d s)
-- type PropagDerivPath d s = Path (PropagDerivLabel d s)

downPropagBoundary ch kid = LeftF (PropagBoundary Down ch) %* [ kid ]
upPropagBoundary ch kid = LeftF (PropagBoundary Up ch) %* [ kid ]

infix 1 downPropagBoundary as ↓
infix 1 upPropagBoundary as ↑

--------------------------------------------------------------------------------
-- PropagDeriv
--------------------------------------------------------------------------------

data PropagRule d s = PropagRule String (Maybe (Tooth (PropagDerivLabel d s)) -> Tree (PropagDerivLabel d s) -> Maybe (Tree (PropagDerivLabel d s)))

type PropagRules d s = List (PropagRule d s)


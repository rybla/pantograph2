module Pantograph.Language where

import Pantograph.Tree
import Prelude

import Control.Plus (empty)
import Data.Eq.Generic (genericEq)
import Data.Function as Function
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, isNothing)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Pantograph.Pretty (class Pretty)
import Pantograph.RevList as RevList
import Pantograph.Utility (todo)

--------------------------------------------------------------------------------
-- SuperLabel
--------------------------------------------------------------------------------

class SuperLabel l_sup l_sub | l_sup -> l_sub where
  injectLabel :: l_sub -> l_sup

--------------------------------------------------------------------------------
-- MetaVar
--------------------------------------------------------------------------------

data MetaVar = MkMetaVar String

derive instance Generic MetaVar _

instance Show MetaVar where
  show x = genericShow x

instance Pretty MetaVar where
  pretty (MkMetaVar str) = "$" <> str

instance Eq MetaVar where
  eq x = genericEq x

instance Ord MetaVar where
  compare x = genericCompare x

--------------------------------------------------------------------------------
-- MetaLabel 
--------------------------------------------------------------------------------

data MetaLabel l
  = MetaVar MetaVar
  | InjectMetaLabel l

derive instance Generic (MetaLabel l) _

instance Show l => Show (MetaLabel l) where
  show x = genericShow x

instance Eq l => Eq (MetaLabel l) where
  eq x = genericEq x

derive instance Functor MetaLabel

instance SuperLabel l l' => SuperLabel (MetaLabel l) l' where
  injectLabel = InjectMetaLabel <<< injectLabel

mkMetaVar x = MetaVar (MkMetaVar x) % Nil
mkMetaVar' x = injectLabel (MetaVar (MkMetaVar x)) % Nil

type MetaVarSubst = Map MetaVar

--------------------------------------------------------------------------------
-- SortLabel
--------------------------------------------------------------------------------

data SortLabel s = InjectSortLabel s

derive instance Generic (SortLabel s) _

instance Show s => Show (SortLabel s) where
  show x = genericShow x

instance Eq s => Eq (SortLabel s) where
  eq x = genericEq x

derive instance Functor SortLabel

instance SuperLabel s s' => SuperLabel (SortLabel s) s' where
  injectLabel = InjectSortLabel <<< injectLabel

class (Show s, Eq s, Pretty s, PrettyTreeLabel s) <= IsSortRuleLabel s

mkTreeInject l kids = injectLabel l % List.fromFoldable kids

infix 2 mkTreeInject as %^

mkCongruenceInject s kids = Congruence (injectLabel s) %* kids

infix 2 mkCongruenceInject as %.^

mkPlusInject s kids_left kid kids_right = Plus (Tooth (injectLabel s) (RevList.fromList (List.fromFoldable kids_left)) (List.fromFoldable kids_right)) %* [ kid ]

infixl 2 mkPlusInject as %+^

mkMinusInject s kids_left kid kids_right = Minus (Tooth (injectLabel s) (RevList.fromList (List.fromFoldable kids_left)) (List.fromFoldable kids_right)) %* [ kid ]

infixl 2 mkMinusInject as %-^

-- | Serves as closing delimeter for mkPlusInject and mkMinusInject
apply' :: forall a b. (a -> b) -> a -> b
apply' = Function.apply

infixl 2 apply' as <<
infixl 2 apply' as >>

--------------------------------------------------------------------------------
-- DerLabel
--------------------------------------------------------------------------------

class (Show d, Eq d, Pretty d, PrettyTreeLabel d) <= IsDerRuleLabel d

type DerLabel d s = DerLabel' d (SortLabel s)

data DerLabel' d s
  = DerLabel d (MetaVarSubst (Tree s))
  | DerBoundary (Tree (ChangeLabel s))

mkDerLabel d sigma = DerLabel d (Map.fromFoldable sigma)

infix 2 mkDerLabel as //

derive instance Generic (DerLabel' d s) _

instance (Show d, Show s) => Show (DerLabel' d s) where
  show x = genericShow x

instance (Eq d, Eq s) => Eq (DerLabel' d s) where
  eq x = genericEq x

derive instance Functor (DerLabel' d)

--------------------------------------------------------------------------------
-- DerRule
--------------------------------------------------------------------------------

newtype DerRule s = DerRule
  { sort :: Tree (MetaLabel (SortLabel s))
  , kids :: List { sort :: Tree (MetaLabel (SortLabel s)) }
  }

mkDerRule sort kids = DerRule { sort, kids: List.fromFoldable (kids # map \sort -> { sort }) }

infix 0 mkDerRule as -|

mkDerRuleFlipped = flip mkDerRule

infix 0 mkDerRuleFlipped as |-

derive instance Generic (DerRule s) _

instance Show s => Show (DerRule s) where
  show x = genericShow x

instance Eq s => Eq (DerRule s) where
  eq x = genericEq x

derive instance Functor DerRule

type DerRules d s = d -> DerRule s

class HasDerRules d s | d -> s where
  derRules :: DerRules d s

class (IsDerRuleLabel d, IsSortRuleLabel s, HasDerRules d s) <= IsLanguage d s | d -> s

--------------------------------------------------------------------------------
-- AdjustLabel
-- TODO: eventually I'll have to deal with the cursor position being somewhere
--------------------------------------------------------------------------------

type AdjustLabel d s = AdjustLabel' d (SortLabel s)

data AdjustLabel' d s
  = AdjustBoundary AdjustBoundaryDirection (Tree (ChangeLabel s))
  | InjectAdjustLabel (DerLabel' d s)

instance PrettyTreeLabel (AdjustLabel' d s) where
  prettyTree = todo ""

data AdjustBoundaryDirection = Up | Down

instance Pretty AdjustBoundaryDirection where
  pretty Up = "↑"
  pretty Down = "↓"

downAdjustBoundary :: forall d s. Tree (ChangeLabel s) -> Tree (AdjustLabel' d s) -> Tree (AdjustLabel' d s)
downAdjustBoundary ch kid = AdjustBoundary Down ch %* [ kid ]

upAdjustBoundary :: forall d s. Tree (ChangeLabel s) -> Tree (AdjustLabel' d s) -> Tree (AdjustLabel' d s)
upAdjustBoundary ch kid = AdjustBoundary Up ch %* [ kid ]

infix 2 downAdjustBoundary as ↓
infix 2 upAdjustBoundary as ↑

--------------------------------------------------------------------------------
-- AdjustRule
--------------------------------------------------------------------------------

newtype AdjustRule d s = AdjustRule
  { name :: String
  , rule :: Maybe (Tooth (AdjustLabel d s)) -> Tree (AdjustLabel d s) -> Maybe (Tree (AdjustLabel d s))
  }

type AdjustRules d s = List (AdjustRule d s)

class HasAdjustRules d s | d -> s where
  propagRules :: AdjustRules d s

--------------------------------------------------------------------------------
-- InsertRule
--------------------------------------------------------------------------------

newtype InsertRule d s = InsertRule
  { name :: String
  , key :: String -- searchable by user
  , rule ::
      Tree (DerLabel d s)
      -> Maybe
           { up :: Tree (ChangeLabel (SortLabel s))
           , mid :: Tooth (DerLabel d s)
           , down :: Tree (ChangeLabel (SortLabel s))
           }
  }

type InsertRules d s = List (InsertRule d s)

class HasInsertRules d s | d -> s where
  insertRules :: InsertRules d s

--------------------------------------------------------------------------------
-- matching
--------------------------------------------------------------------------------

matchChange
  :: forall l
   . Eq l
  => Tree (MetaLabel (ChangeLabel l))
  -> Tree (ChangeLabel l)
  -> Maybe (MetaVarSubst (Tree (ChangeLabel l)))
matchChange (MetaVar x % Nil) t2 = pure $ Map.singleton x t2
matchChange (InjectMetaLabel l1 % ts1) (l2 % ts2) | l1 == l2 = List.zipWithA matchChange ts1 ts2 >>= List.foldM mergeMetaVarSubsts Map.empty
matchChange _ _ = empty

mergeMetaVarSubsts :: forall a. Eq a => MetaVarSubst a -> MetaVarSubst a -> Maybe (MetaVarSubst a)
mergeMetaVarSubsts s1 s2 = List.foldM f s1 (Map.toUnfoldable s2)
  where
  f m (x /\ a) | m # Map.lookup x # isNothing = m # Map.insert x a # pure
  f _ _ = empty


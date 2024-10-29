module Pantograph.Language where

import Pantograph.Tree
import Prelude

import Control.Plus (empty)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable)
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

instance SuperLabel (MetaLabel l) l where
  injectLabel = InjectMetaLabel

mkMetaVar :: forall l. String -> Tree (MetaLabel l)
mkMetaVar x = MetaVar (MkMetaVar x) % Nil

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

instance SuperLabel (SortLabel s) s where
  injectLabel = InjectSortLabel

class (Show s, Eq s, Pretty s, PrettyTreeLabel s) <= IsSortRuleLabel s

mkSort :: forall f s. Foldable f => s -> f (Tree (SortLabel s)) -> Tree (SortLabel s)
mkSort s kids = InjectSortLabel s %* kids

infix 1 mkSort as %^

mkTreeInject l kids = injectLabel l % List.fromFoldable kids

infix 1 mkTreeInject as %|^

mkCongruenceInject s kids = Congruence (injectLabel s) %* kids

infix 1 mkCongruenceInject as %∂.^

mkPlusInject s kids_left kid kids_right = Plus (Tooth (injectLabel s) (RevList.fromList (List.fromFoldable kids_left)) (List.fromFoldable kids_right)) %* [ kid ]

infixl 1 mkPlusInject as %∂+^

mkMinusInject s kids_left kid kids_right = Minus (Tooth (injectLabel s) (RevList.fromList (List.fromFoldable kids_left)) (List.fromFoldable kids_right)) %* [ kid ]

infixl 1 mkMinusInject as %∂-^

-- | Serves as closing delimeter for mkPlusInject and mkMinusInject
apply' :: forall a b. (a -> b) -> a -> b
apply' = Function.apply

infixl 1 apply' as <<
infixl 1 apply' as >>

--------------------------------------------------------------------------------
-- DerivLabel
--------------------------------------------------------------------------------

class (Show d, Eq d, Pretty d, PrettyTreeLabel d) <= IsDerivRuleLabel d

type DerivLabel d s = DerivLabel' d (SortLabel s)

data DerivLabel' d s
  = DerivLabel d (MetaVarSubst (Tree s))
  | DerivBoundary (Tree (ChangeLabel s))

mkDerivLabel d sigma = DerivLabel d (Map.fromFoldable sigma)

infix 1 mkDerivLabel as //

derive instance Generic (DerivLabel' d s) _

instance (Show d, Show s) => Show (DerivLabel' d s) where
  show x = genericShow x

instance (Eq d, Eq s) => Eq (DerivLabel' d s) where
  eq x = genericEq x

derive instance Functor (DerivLabel' d)

--------------------------------------------------------------------------------
-- DerivRule
--------------------------------------------------------------------------------

newtype DerivRule s = DerivRule
  { sort :: Tree (MetaLabel (SortLabel s))
  , kids :: List { sort :: Tree (MetaLabel (SortLabel s)) }
  }

derive instance Generic (DerivRule s) _

instance Show s => Show (DerivRule s) where
  show x = genericShow x

instance Eq s => Eq (DerivRule s) where
  eq x = genericEq x

derive instance Functor DerivRule

type DerivRules d s = d -> DerivRule s

class HasDerivRules d s | d -> s where
  derivRules :: DerivRules d s

class (IsDerivRuleLabel d, IsSortRuleLabel s, HasDerivRules d s) <= IsLanguage d s | d -> s

--------------------------------------------------------------------------------
-- DerivChangeRule 
--------------------------------------------------------------------------------

newtype DerivChangeRule s = DerivChangeRule
  { kids :: List { change :: Tree (ChangeLabel (SortLabel s)) } }

derive instance Generic (DerivChangeRule s) _

instance Show s => Show (DerivChangeRule s) where
  show x = genericShow x

instance Eq s => Eq (DerivChangeRule s) where
  eq x = genericEq x

derive instance Functor DerivChangeRule

type DerivChangeRules d s = d -> DerivChangeRule s

class HasDerivChangeRules d s | d -> s where
  derivChangeRules :: DerivChangeRules d s

--------------------------------------------------------------------------------
-- PropagLabel
-- TODO: eventually I'll have to deal with the cursor position being somewhere in the Propag
--------------------------------------------------------------------------------

data PropagLabel d s = PropagLabel d (SortLabel s)

data PropagLabel' d s
  = PropagBoundary PropagBoundaryDirection (Tree (ChangeLabel s))
  | InjectPropagLabel (DerivLabel' d s)

data PropagBoundaryDirection = Up | Down

downPropagBoundary :: forall d s. Tree (ChangeLabel s) -> Tree (PropagLabel' d s) -> Tree (PropagLabel' d s)
downPropagBoundary ch kid = PropagBoundary Down ch %* [ kid ]

upPropagBoundary :: forall d s. Tree (ChangeLabel s) -> Tree (PropagLabel' d s) -> Tree (PropagLabel' d s)
upPropagBoundary ch kid = PropagBoundary Up ch %* [ kid ]

infix 1 downPropagBoundary as ↓
infix 1 upPropagBoundary as ↑

--------------------------------------------------------------------------------
-- PropagRule
--------------------------------------------------------------------------------

newtype PropagRule d s = PropagRule
  { name :: String
  , rule :: Maybe (Tooth (PropagLabel d s)) -> Tree (PropagLabel d s) -> Maybe (Tree (PropagLabel d s))
  }

type PropagRules d s = List (PropagRule d s)

class HasPropagRules d s | d -> s where
  propagRules :: PropagRules d s

--------------------------------------------------------------------------------
-- InsertRule
--------------------------------------------------------------------------------

newtype InsertRule d s = InsertRule
  { name :: String
  , key :: String -- searchable by user
  , rule ::
      Tree (DerivLabel d s)
      -> Maybe
           { up :: Tree (ChangeLabel (SortLabel s))
           , mid :: Tooth (DerivLabel d s)
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


module Pantograph.Language where

import Pantograph.Tree
import Prelude

import Control.Plus (empty)
import Data.Eq.Generic (genericEq)
import Data.Function as Function
import Data.Functor.Variant (VariantF)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe', isNothing, maybe, maybe')
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Pantograph.Pretty (class Pretty)
import Pantograph.RevList as RevList
import Pantograph.Utility (todo)

--------------------------------------------------------------------------------
-- SuperLbl
--------------------------------------------------------------------------------

class SuperLbl l_sup l_sub | l_sup -> l_sub where
  injectLbl :: l_sub -> l_sup

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
-- MetaLbl 
--------------------------------------------------------------------------------

data MetaLbl l
  = MetaVar MetaVar
  | InjMetaLbl l

derive instance Generic (MetaLbl l) _

instance Show l => Show (MetaLbl l) where
  show x = genericShow x

instance Eq l => Eq (MetaLbl l) where
  eq x = genericEq x

derive instance Functor MetaLbl

instance SuperLbl l l' => SuperLbl (MetaLbl l) l' where
  injectLbl = InjMetaLbl <<< injectLbl

mkMetaVar x = MetaVar (MkMetaVar x) % Nil
mkMetaVar' x = injectLbl (MetaVar (MkMetaVar x)) % Nil

type MetaVarSubst = Map MetaVar

--------------------------------------------------------------------------------
-- SortLbl
--------------------------------------------------------------------------------

data SortLbl s = InjSortLbl s

derive instance Generic (SortLbl s) _

instance Show s => Show (SortLbl s) where
  show x = genericShow x

instance Eq s => Eq (SortLbl s) where
  eq x = genericEq x

derive instance Functor SortLbl

instance SuperLbl s s' => SuperLbl (SortLbl s) s' where
  injectLbl = InjSortLbl <<< injectLbl

class (Show s, Eq s, Pretty s, PrettyTreeLbl s) <= IsSortRuleLbl s

mkTreeInj l kids = injectLbl l % List.fromFoldable kids

infix 2 mkTreeInj as %^

type ChangeSortLbl s = ChangeLbl (SortLbl s)

mkCongruenceInj s kids = Congruence (injectLbl s) %* kids

infix 2 mkCongruenceInj as %.^

mkPlusInj s kids_left kid kids_right = Plus (Tooth (injectLbl s) (RevList.fromList (List.fromFoldable kids_left)) (List.fromFoldable kids_right)) %* [ kid ]

infixl 2 mkPlusInj as %+^

mkMinusInj s kids_left kid kids_right = Minus (Tooth (injectLbl s) (RevList.fromList (List.fromFoldable kids_left)) (List.fromFoldable kids_right)) %* [ kid ]

infixl 2 mkMinusInj as %-^

-- | Serves as closing delimeter for mkPlusInj and mkMinusInj
apply' :: forall a b. (a -> b) -> a -> b
apply' = Function.apply

infixl 2 apply' as <<
infixl 2 apply' as >>

--------------------------------------------------------------------------------
-- DerLbl
--------------------------------------------------------------------------------

class (Show d, Eq d, Pretty d, PrettyTreeLbl d) <= IsDerRuleLbl d

type DerLbl d s = DerLbl' d (SortLbl s)

data DerLbl' d s = DerLbl d (MetaVarSubst (Tree s))

mkDerLbl d sigma = DerLbl d (Map.fromFoldable sigma)

infix 2 mkDerLbl as //

derive instance Generic (DerLbl' d s) _

instance (Show d, Show s) => Show (DerLbl' d s) where
  show x = genericShow x

instance (Eq d, Eq s) => Eq (DerLbl' d s) where
  eq x = genericEq x

derive instance Functor (DerLbl' d)

--------------------------------------------------------------------------------
-- DerRule
--------------------------------------------------------------------------------

newtype DerRule s = DerRule
  { sort :: Tree (MetaLbl (SortLbl s))
  , kids :: List { sort :: Tree (MetaLbl (SortLbl s)) }
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

class (IsDerRuleLbl d, IsSortRuleLbl s, HasDerRules d s) <= IsLanguage d s | d -> s

instance SuperLbl (DerLbl' d s) (DerLbl' d s) where
  injectLbl = identity

--------------------------------------------------------------------------------
-- AdjLbl
-- TODO: eventually I'll have to deal with the cursor position being somewhere
--------------------------------------------------------------------------------

type AdjLbl d s = AdjLbl' d (SortLbl s)

data AdjLbl' d s
  = AdjBdry BdryDir (Tree (ChangeLbl s))
  | InjAdjLbl (DerLbl' d s)

derive instance Generic (AdjLbl' d s) _

instance (Eq d, Eq s) => Eq (AdjLbl' d s) where
  eq x = genericEq x

instance PrettyTreeLbl (AdjLbl' d s) where
  prettyTree = todo ""

instance SuperLbl (DerLbl' d s) l' => SuperLbl (AdjLbl' d s) l' where
  injectLbl = InjAdjLbl <<< injectLbl

data BdryDir = Up | Down

instance Pretty BdryDir where
  pretty Up = "↑"
  pretty Down = "↓"

derive instance Generic BdryDir _

instance Eq BdryDir where
  eq x = genericEq x

instance Ord BdryDir where
  compare x = genericCompare x

downAdjBdry :: forall d s. Tree (ChangeLbl s) -> Tree (AdjLbl' d s) -> Tree (AdjLbl' d s)
downAdjBdry ch kid = AdjBdry Down ch %* [ kid ]

upAdjBdry :: forall d s. Tree (ChangeLbl s) -> Tree (AdjLbl' d s) -> Tree (AdjLbl' d s)
upAdjBdry ch kid = AdjBdry Up ch %* [ kid ]

infix 2 downAdjBdry as ↓
infix 2 upAdjBdry as ↑

--------------------------------------------------------------------------------
-- AdjRule
--------------------------------------------------------------------------------

newtype AdjRules d s = AdjRules
  { upTopRule :: UpTopRule d s
  , upRules :: List (UpAdjRule d s)
  , downRules :: List (DownAdjRule d s)
  }

instance Semigroup (AdjRules d s) where
  append (AdjRules ars1) (AdjRules ars2) = AdjRules
    { upTopRule: \ch_and_t -> ars1.upTopRule ch_and_t # flip maybe' pure \_ -> ars2.upTopRule ch_and_t
    , upRules: ars1.upRules <> ars2.upRules
    , downRules: ars1.downRules <> ars2.downRules
    }

instance Monoid (AdjRules d s) where
  mempty = AdjRules
    { upTopRule: const empty
    , upRules: mempty
    , downRules: mempty
    }

type DownAdjRule d s = Tree (ChangeSortLbl s) /\ Tree (AdjLbl d s) -> Maybe { up :: Maybe (Tree (ChangeSortLbl s)), mid :: Path (AdjLbl d s), down :: Maybe (Tree (ChangeSortLbl s)) }
type UpAdjRule d s = Tooth (AdjLbl d s) /\ Tree (ChangeSortLbl s) -> Maybe { up :: Maybe (Tree (ChangeSortLbl s)), mid :: Path (AdjLbl d s), down :: Maybe (Tree (ChangeSortLbl s)) }
type UpTopRule d s = Tree (ChangeSortLbl s) /\ Tree (AdjLbl d s) -> Maybe (Tree (AdjLbl d s))

class HasAdjRules d s | d -> s where
  adjRules :: AdjRules d s

--------------------------------------------------------------------------------
-- InsertRule
--------------------------------------------------------------------------------

newtype InsertRule d s = InsertRule
  { name :: String
  , key :: String -- searchable by user
  , rule ::
      Tree (DerLbl d s)
      -> Maybe
           { up :: Tree (ChangeSortLbl s)
           , mid :: Tooth (DerLbl d s)
           , down :: Tree (ChangeSortLbl s)
           }
  }

type InsertRules d s = List (InsertRule d s)

class HasInsertRules d s | d -> s where
  insertRules :: InsertRules d s

--------------------------------------------------------------------------------
-- matching
--------------------------------------------------------------------------------

matchMetaTree
  :: forall l
   . Eq l
  => Tree (MetaLbl l)
  -> Tree l
  -> Maybe (MetaVarSubst (Tree l))
matchMetaTree (MetaVar x % Nil) t2 = pure $ Map.singleton x t2
matchMetaTree (InjMetaLbl l1 % ts1) (l2 % ts2) | l1 == l2 = List.zipWithA matchMetaTree ts1 ts2 >>= List.foldM mergeMetaVarSubsts Map.empty
matchMetaTree _ _ = empty

mergeMetaVarSubsts :: forall a. Eq a => MetaVarSubst a -> MetaVarSubst a -> Maybe (MetaVarSubst a)
mergeMetaVarSubsts s1 s2 = List.foldM f s1 (Map.toUnfoldable s2)
  where
  f m (x /\ a) | m # Map.lookup x # isNothing = m # Map.insert x a # pure
  f _ _ = empty

matchTreeSortLbl
  :: forall s
   . IsSortRuleLbl s
  => Tree (MetaLbl (SortLbl s))
  -> Tree (SortLbl s)
  -> Maybe (MetaVarSubst (Tree (SortLbl s)))
matchTreeSortLbl ms s = todo "matchTreeSortLbl"

matchTreeChangeSortLbl
  :: forall s
   . IsSortRuleLbl s
  => Tree (MetaLbl (ChangeSortLbl s))
  -> Tree (ChangeSortLbl s)
  -> Maybe (MetaVarSubst (Tree (ChangeSortLbl s)))
matchTreeChangeSortLbl mc c = todo "matchTreeChangeSortLbl"

matchTreeDerLbl
  :: forall d s
   . IsLanguage d s
  => Tree (MetaLbl (DerLbl' d (MetaLbl (SortLbl s))))
  -> Tree (DerLbl d s)
  -> Maybe (MetaVarSubst (Tree (DerLbl d s)) /\ MetaVarSubst (Tree (ChangeSortLbl s)) /\ MetaVarSubst (Tree (SortLbl s)))
matchTreeDerLbl mt t = todo "matchTreeDerLbl"

matchTreeAdjLbl
  :: forall d s
   . IsLanguage d s
  => Tree (MetaLbl (AdjLbl' d (MetaLbl (SortLbl s))))
  -> Tree (AdjLbl d s)
  -> Maybe (MetaVarSubst (Tree (AdjLbl d s)) /\ MetaVarSubst (Tree (SortLbl s)))
matchTreeAdjLbl mt t = todo "matchTreeAdjLbl"
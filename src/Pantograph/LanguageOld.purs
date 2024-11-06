module Pantograph.LanguageOld where

import Pantograph.Tree
import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.State (execStateT, gets, modify_)
import Control.Plus (empty)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, traverse_)
import Data.Function as Function
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (class Traversable)
import Data.Tuple (uncurry)
import Foreign.Object (Object)
import Foreign.Object as Object
import Pantograph.EitherF (EitherF(..))
import Pantograph.Pretty (class Pretty, parens, pretty)
import Pantograph.RevList as RevList
import Pantograph.Utility (class FromObjectToRecord, bug, fromObjectToRecord)
import Prim.Row (class Cons, class Lacks)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record as Record
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------
-- RulialVar
--------------------------------------------------------------------------------

data RulialVar = RulialVar String

mkRulialVar :: forall a. String -> Tree (Either RulialVar a)
mkRulialVar x = Left (RulialVar x) % Nil

type Rulial = Either RulialVar

type RulialVarSubst = Map RulialVar

--------------------------------------------------------------------------------
-- SortLbl
--------------------------------------------------------------------------------

class (Show s, Eq s, Pretty s, PrettyTreeLbl s) <= IsSortRuleLbl s

data SortLbl s = SortLbl s

mkSort :: forall f s. Foldable f => s -> f (Tree (SortLbl s)) -> Tree (SortLbl s)
mkSort s kids = SortLbl s %* kids

infix 1 mkSort as %^

mkRightSort :: forall a s f. Foldable f => s -> f (Tree (Either a (SortLbl s))) -> Tree (Either a (SortLbl s))
mkRightSort s kids = Right (SortLbl s) % List.fromFoldable kids

infix 1 mkRightSort as %|^

mkCongruenceSort :: forall f s. Foldable f => s -> f (Tree (ChangeLbl (SortLbl s))) -> Tree (ChangeLbl (SortLbl s))
mkCongruenceSort s kids = Congruence (SortLbl s) %* kids

infix 1 mkCongruenceSort as %∂.^

mkPlusSort :: forall s f1 f2. Foldable f1 => Foldable f2 => s -> f1 (Tree (SortLbl s)) -> Tree (ChangeLbl (SortLbl s)) -> f2 (Tree (SortLbl s)) -> Tree (ChangeLbl (SortLbl s))
mkPlusSort s kids_left kid kids_right = Plus (Tooth (SortLbl s) (RevList.fromList (List.fromFoldable kids_left)) (List.fromFoldable kids_right)) %* [ kid ]

infixl 1 mkPlusSort as %∂+^

mkMinusSort :: forall s f1 f2. Foldable f1 => Foldable f2 => s -> f1 (Tree (SortLbl s)) -> Tree (ChangeLbl (SortLbl s)) -> f2 (Tree (SortLbl s)) -> Tree (ChangeLbl (SortLbl s))
mkMinusSort s kids_left kid kids_right = Minus (Tooth (SortLbl s) (RevList.fromList (List.fromFoldable kids_left)) (List.fromFoldable kids_right)) %* [ kid ]

infixl 1 mkMinusSort as %∂-^

mkReplaceSort :: forall s. Tree (SortLbl s) -> Tree (SortLbl s) -> Tree (ChangeLbl (SortLbl s))
mkReplaceSort t1 t2 = Replace t1 t2 %* []

infix 1 mkReplaceSort as %∂~>^

-- | Serves as closing delimeter for mkPlusSort and mkMinusSort
apply' :: forall a b. (a -> b) -> a -> b
apply' = Function.apply

infixl 1 apply' as <<
infixl 1 apply' as >>

--------------------------------------------------------------------------------
-- DerLbl
--------------------------------------------------------------------------------

class (Show d, Eq d, Pretty d, PrettyTreeLbl d) <= IsDerivRuleLbl d

type DerLbl d s = DerLbl' d (SortLbl s)

data DerLbl' d s
  = DerLbl d (RulialVarSubst (Tree s))
  | DerBdry (Tree (ChangeLbl s))

--------------------------------------------------------------------------------
-- DerivRule
--------------------------------------------------------------------------------

newtype DerivRule s = DerivRule
  { sort :: Tree (Rulial (SortLbl s))
  , kids :: List { sort :: Tree (Rulial (SortLbl s)) }
  }

type DerRules d s = d -> DerivRule s

class HasDerRules d s | d -> s where
  derRules :: DerRules d s

class (IsDerivRuleLbl d, IsSortRuleLbl s, HasDerRules d s) <= IsLanguage d s | d -> s

--------------------------------------------------------------------------------
-- DerChangeRule 
--------------------------------------------------------------------------------

newtype DerChangeRule s = DerChangeRule
  { kids :: List { change :: Tree (ChangeLbl (SortLbl s)) } }

type DerChangeRules d s = d -> DerChangeRule s

class HasDerChangeRules d s | d -> s where
  derChangeRules :: DerChangeRules d s

class (IsLanguage d s, HasDerChangeRules d s) <= IsDerChangeLanguage d s | d -> s

--------------------------------------------------------------------------------
-- DerAdjustRule
--------------------------------------------------------------------------------

newtype DerAdjustRule s = DerAdjustRule
  { kids ::
      List
        { passthrough_down :: Tree (ChangeLbl (SortLbl s)) -> Maybe (Tree (ChangeLbl (SortLbl s)))
        , passthrough_up :: Tree (ChangeLbl (SortLbl s)) -> Maybe (Tree (ChangeLbl (SortLbl s)))
        , wrap_down :: Tree (ChangeLbl (SortLbl s)) -> Maybe { up :: Maybe (Tree (ChangeLbl (SortLbl s))), down :: Maybe (Tree (ChangeLbl (SortLbl s))) }
        , wrap_up :: Tree (ChangeLbl (SortLbl s)) -> Maybe { up :: Maybe (Tree (ChangeLbl (SortLbl s))), down :: Maybe (Tree (ChangeLbl (SortLbl s))) }
        , unwrap_down :: Tree (ChangeLbl (SortLbl s)) -> Maybe { up :: Maybe (Tree (ChangeLbl (SortLbl s))), down :: Maybe (Tree (ChangeLbl (SortLbl s))) }
        , unwrap_up :: Tree (ChangeLbl (SortLbl s)) -> Maybe { up :: Maybe (Tree (ChangeLbl (SortLbl s))), down :: Maybe (Tree (ChangeLbl (SortLbl s))) }
        }
  }

type DerAdjustRules d s = d -> DerAdjustRule s

class HasDerAdjustRules d s | d -> s where
  derAdjustRules :: DerAdjustRules d s

class (IsLanguage d s, HasDerAdjustRules d s) <= IsDerAdjustLanguage d s | d -> s

--------------------------------------------------------------------------------
-- AdjustLbl
-- TODO: eventually I'll have to deal with the cursor position being somewhere in the Adjust
--------------------------------------------------------------------------------

type AdjustLbl d s = AdjustLbl' (DerLbl' d) (SortLbl s)

type AdjustLbl' = EitherF AdjustLbl''

data AdjustLbl'' s = AdjustBdry BdryDirection (Tree (ChangeLbl s))

data BdryDirection = Up | Down

downAdjustBdry :: forall d s. Tree (ChangeLbl s) -> Tree (AdjustLbl' d s) -> Tree (AdjustLbl' d s)
downAdjustBdry ch kid = LeftF (AdjustBdry Down ch) %* [ kid ]

upAdjustBdry :: forall d s. Tree (ChangeLbl s) -> Tree (AdjustLbl' d s) -> Tree (AdjustLbl' d s)
upAdjustBdry ch kid = LeftF (AdjustBdry Up ch) %* [ kid ]

infix 1 downAdjustBdry as ↓
infix 1 upAdjustBdry as ↑

--------------------------------------------------------------------------------
-- AdjustRule
--------------------------------------------------------------------------------

newtype AdjustRule d s = AdjustRule
  { name :: String
  , rule :: Maybe (Tooth (AdjustLbl d s)) -> Tree (AdjustLbl d s) -> Maybe (Tree (AdjustLbl d s))
  }

type AdjustRules d s = List (AdjustRule d s)

class HasAdjustRules d s | d -> s where
  adjustRules :: AdjustRules d s

class (IsLanguage d s, HasAdjustRules d s) <= IsAdjustLanguage d s | d -> s

--------------------------------------------------------------------------------
-- InsertRule
--------------------------------------------------------------------------------

newtype InsertRule d s = InsertRule
  { name :: String
  , key :: String -- searchable by user
  , rule ::
      Tree (DerLbl d s)
      -> Maybe
           { up :: Tree (ChangeLbl (SortLbl s))
           , mid :: Tooth (DerLbl d s)
           , down :: Tree (ChangeLbl (SortLbl s))
           }
  }

type InsertRules d s = List (InsertRule d s)

class HasInsertRules d s | d -> s where
  insertRules :: InsertRules d s

class (IsAdjustLanguage d s, HasInsertRules d s) <= IsInsertLanguage d s | d -> s

--------------------------------------------------------------------------------
-- matchTreeChangeSort
--------------------------------------------------------------------------------

reflectExistsProxyCons :: forall xs a. ExistsProxyCons xs a -> String
reflectExistsProxyCons = runExistsProxyCons reflectSymbol

newtype ExistsProxyCons :: Row Type -> Type -> Type
newtype ExistsProxyCons xs a = ExistsProxyCons (forall r. ExistsProxyConsK xs a r -> r)

type ExistsProxyConsK :: Row Type -> Type -> Type -> Type
type ExistsProxyConsK xs a r = forall x xs_. IsSymbol x => Cons x a xs_ xs => Proxy x -> r

mkExistsProxyCons :: forall xs a. ExistsProxyConsK xs a (ExistsProxyCons xs a)
mkExistsProxyCons a = ExistsProxyCons \k -> k a

runExistsProxyCons :: forall xs a r. ExistsProxyConsK xs a r -> ExistsProxyCons xs a -> r
runExistsProxyCons k1 (ExistsProxyCons k2) = k2 k1

type Matchial :: Row Type -> Type -> Type -> Type
type Matchial xs a b = Either (ExistsProxyCons xs a) b

matchTreeChangeSort
  :: forall xs s a
   . IsSortRuleLbl s
  => FromObjectToRecord (Tree (ChangeLbl (SortLbl s))) xs
  => Tree (Matchial xs (Tree (ChangeLbl (SortLbl s))) (ChangeLbl (SortLbl s)))
  -> (Record xs -> a)
  -> Tree (ChangeLbl (SortLbl s))
  -> Maybe a
matchTreeChangeSort t1_ k t2_ =
  go t1_ t2_
    # flip execStateT (Object.empty :: Object (Tree (ChangeLbl (SortLbl s))))
    # runMaybeT
    # (unwrap :: Identity _ -> _)
    # bindFlipped fromObjectToRecord
    # map k
  where
  go (Left x1 % _) t2 = do
    let s = reflectExistsProxyCons x1
    gets (Object.lookup s) >>= case _ of
      Nothing -> modify_ $ Object.insert (reflectExistsProxyCons x1) t2
      -- if x1 has already been matched, then must be matched to the same value
      Just t2' | t2 == t2' -> pure unit
      _ -> empty
  go (Right l1 % ts1) (l2 % ts2) | l1 == l2 = List.zip ts1 ts2 # traverse_ (uncurry go)
  go _ _ = empty

matchialVar
  :: forall x xs_ xs s a
   . IsSymbol x
  => Cons x (Tree (ChangeLbl (SortLbl s))) xs_ xs
  => Proxy x
  -> Tree (Matchial xs (Tree (ChangeLbl (SortLbl s))) a)
matchialVar x = Left (mkExistsProxyCons x) %* []

mkMatchialCongruenceSort :: forall s xs a. s -> Array (Tree (Matchial xs a (ChangeLbl (SortLbl s)))) -> Tree (Matchial xs a (ChangeLbl (SortLbl s)))
mkMatchialCongruenceSort s kids = Right (Congruence (SortLbl s)) % List.fromFoldable kids

infix 1 mkMatchialCongruenceSort as %|∂.^

mkMatchialPlusSort :: forall s xs a f1 f2. Foldable f1 => Foldable f2 => s -> f1 (Tree (SortLbl s)) -> Tree (Matchial xs a (ChangeLbl (SortLbl s))) -> f2 (Tree (SortLbl s)) -> Tree (Matchial xs a (ChangeLbl (SortLbl s)))
mkMatchialPlusSort s kids_left kid kids_right = Right (Plus (Tooth (SortLbl s) (RevList.fromList (List.fromFoldable kids_left)) (List.fromFoldable kids_right))) %* [ kid ]

infixl 1 mkMatchialPlusSort as %|∂+^

mkMatchialMinusSort :: forall s xs a f1 f2. Foldable f1 => Foldable f2 => s -> f1 (Tree (SortLbl s)) -> Tree (Matchial xs a (ChangeLbl (SortLbl s))) -> f2 (Tree (SortLbl s)) -> Tree (Matchial xs a (ChangeLbl (SortLbl s)))
mkMatchialMinusSort s kids_left kid kids_right = Right (Minus (Tooth (SortLbl s) (RevList.fromList (List.fromFoldable kids_left)) (List.fromFoldable kids_right))) %* [ kid ]

infixl 1 mkMatchialMinusSort as %|∂-^

--------------------------------------------------------------------------------
-- instances
--------------------------------------------------------------------------------

derive instance Generic RulialVar _

instance Show RulialVar where
  show x = genericShow x

instance Pretty RulialVar where
  pretty (RulialVar str) = "$" <> str

instance PrettyTreeLbl RulialVar where
  prettyTree rv Nil = pretty rv
  prettyTree _ _ = bug "invalid `Tree RulialVar`"

instance Eq RulialVar where
  eq x = genericEq x

instance Ord RulialVar where
  compare x = genericCompare x

derive instance Generic (SortLbl s) _

instance Pretty s => Pretty (SortLbl s) where
  pretty (SortLbl t) = pretty t

instance PrettyTreeLbl s => PrettyTreeLbl (SortLbl s) where
  prettyTree (SortLbl s) = prettyTree s

instance Show s => Show (SortLbl s) where
  show x = genericShow x

instance Eq s => Eq (SortLbl s) where
  eq x = genericEq x

derive instance Functor SortLbl
derive instance Foldable SortLbl
derive instance Traversable SortLbl

derive instance Generic (DerLbl' d s) _

instance (Show s, Show d) => Show (DerLbl' d s) where
  show x = genericShow x

instance (PrettyTreeLbl s, Pretty d) => Pretty (DerLbl' d s) where
  pretty (DerLbl d sigma) = parens $ pretty d <> " " <> pretty sigma
  pretty (DerBdry ch) = parens $ "!! " <> pretty ch

instance (PrettyTreeLbl s, PrettyTreeLbl d) => PrettyTreeLbl (DerLbl' d s) where
  prettyTree (DerLbl d _sigma) kids = prettyTree d kids
  prettyTree (DerBdry ch) (kid : Nil) = parens $ pretty ch <> " !! " <> kid
  prettyTree _ _ = bug "invalid `Tree (DerLbl' d s)`"

instance (Eq s, Eq d) => Eq (DerLbl' d s) where
  eq x = genericEq x

derive instance Functor (DerLbl' d)

derive instance Generic (DerivRule s) _

derive instance Newtype (DerivRule s) _

derive instance Generic (DerAdjustRule s) _

derive instance Newtype (DerAdjustRule s) _

derive instance Generic (AdjustLbl'' s) _

instance Show s => Show (AdjustLbl'' s) where
  show x = genericShow x

instance PrettyTreeLbl s => Pretty (AdjustLbl'' s) where
  pretty (AdjustBdry dir ch) = parens $ "!! " <> pretty dir <> " " <> pretty ch

instance PrettyTreeLbl s => PrettyTreeLbl (AdjustLbl'' s) where
  prettyTree (AdjustBdry dir ch) (kid : Nil) = parens $ pretty ch <> " " <> pretty dir <> "  " <> kid
  prettyTree _ _ = bug "invalid `AdjustLbl' s`"

instance Eq s => Eq (AdjustLbl'' s) where
  eq x = genericEq x

derive instance Functor AdjustLbl''

derive instance Generic BdryDirection _

instance Show BdryDirection where
  show x = genericShow x

instance Pretty BdryDirection where
  pretty Up = "↑"
  pretty Down = "↓"

instance Eq BdryDirection where
  eq x = genericEq x

derive instance Generic (AdjustRule d s) _

derive instance Newtype (AdjustRule d s) _


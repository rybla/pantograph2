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
-- SortLabel
--------------------------------------------------------------------------------

class (Show s, Eq s, Pretty s, PrettyTreeLabel s) <= IsSortRuleLabel s

data SortLabel s = SortLabel s

mkSort :: forall f s. Foldable f => s -> f (Tree (SortLabel s)) -> Tree (SortLabel s)
mkSort s kids = SortLabel s %* kids

infix 1 mkSort as %^

mkRightSort :: forall a s f. Foldable f => s -> f (Tree (Either a (SortLabel s))) -> Tree (Either a (SortLabel s))
mkRightSort s kids = Right (SortLabel s) % List.fromFoldable kids

infix 1 mkRightSort as %|^

mkCongruenceSort :: forall f s. Foldable f => s -> f (Tree (ChangeLabel (SortLabel s))) -> Tree (ChangeLabel (SortLabel s))
mkCongruenceSort s kids = Congruence (SortLabel s) %* kids

infix 1 mkCongruenceSort as %∂.^

mkPlusSort :: forall s f1 f2. Foldable f1 => Foldable f2 => s -> f1 (Tree (SortLabel s)) -> Tree (ChangeLabel (SortLabel s)) -> f2 (Tree (SortLabel s)) -> Tree (ChangeLabel (SortLabel s))
mkPlusSort s kids_left kid kids_right = Plus (Tooth (SortLabel s) (RevList.fromList (List.fromFoldable kids_left)) (List.fromFoldable kids_right)) %* [ kid ]

infixl 1 mkPlusSort as %∂+^

mkMinusSort :: forall s f1 f2. Foldable f1 => Foldable f2 => s -> f1 (Tree (SortLabel s)) -> Tree (ChangeLabel (SortLabel s)) -> f2 (Tree (SortLabel s)) -> Tree (ChangeLabel (SortLabel s))
mkMinusSort s kids_left kid kids_right = Minus (Tooth (SortLabel s) (RevList.fromList (List.fromFoldable kids_left)) (List.fromFoldable kids_right)) %* [ kid ]

infixl 1 mkMinusSort as %∂-^

mkReplaceSort :: forall s. Tree (SortLabel s) -> Tree (SortLabel s) -> Tree (ChangeLabel (SortLabel s))
mkReplaceSort t1 t2 = Replace t1 t2 %* []

infix 1 mkReplaceSort as %∂~>^

-- | Serves as closing delimeter for mkPlusSort and mkMinusSort
apply' :: forall a b. (a -> b) -> a -> b
apply' = Function.apply

infixl 1 apply' as <<
infixl 1 apply' as >>

--------------------------------------------------------------------------------
-- DerLabel
--------------------------------------------------------------------------------

class (Show d, Eq d, Pretty d, PrettyTreeLabel d) <= IsDerivRuleLabel d

type DerLabel d s = DerLabel' d (SortLabel s)

data DerLabel' d s
  = DerLabel d (RulialVarSubst (Tree s))
  | DerBoundary (Tree (ChangeLabel s))

--------------------------------------------------------------------------------
-- DerivRule
--------------------------------------------------------------------------------

newtype DerivRule s = DerivRule
  { sort :: Tree (Rulial (SortLabel s))
  , kids :: List { sort :: Tree (Rulial (SortLabel s)) }
  }

type DerRules d s = d -> DerivRule s

class HasDerRules d s | d -> s where
  derRules :: DerRules d s

class (IsDerivRuleLabel d, IsSortRuleLabel s, HasDerRules d s) <= IsLanguage d s | d -> s

--------------------------------------------------------------------------------
-- DerChangeRule 
--------------------------------------------------------------------------------

newtype DerChangeRule s = DerChangeRule
  { kids :: List { change :: Tree (ChangeLabel (SortLabel s)) } }

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
        { passthrough_down :: Tree (ChangeLabel (SortLabel s)) -> Maybe (Tree (ChangeLabel (SortLabel s)))
        , passthrough_up :: Tree (ChangeLabel (SortLabel s)) -> Maybe (Tree (ChangeLabel (SortLabel s)))
        , wrap_down :: Tree (ChangeLabel (SortLabel s)) -> Maybe { up :: Maybe (Tree (ChangeLabel (SortLabel s))), down :: Maybe (Tree (ChangeLabel (SortLabel s))) }
        , wrap_up :: Tree (ChangeLabel (SortLabel s)) -> Maybe { up :: Maybe (Tree (ChangeLabel (SortLabel s))), down :: Maybe (Tree (ChangeLabel (SortLabel s))) }
        , unwrap_down :: Tree (ChangeLabel (SortLabel s)) -> Maybe { up :: Maybe (Tree (ChangeLabel (SortLabel s))), down :: Maybe (Tree (ChangeLabel (SortLabel s))) }
        , unwrap_up :: Tree (ChangeLabel (SortLabel s)) -> Maybe { up :: Maybe (Tree (ChangeLabel (SortLabel s))), down :: Maybe (Tree (ChangeLabel (SortLabel s))) }
        }
  }

type DerAdjustRules d s = d -> DerAdjustRule s

class HasDerAdjustRules d s | d -> s where
  derAdjustRules :: DerAdjustRules d s

class (IsLanguage d s, HasDerAdjustRules d s) <= IsDerAdjustLanguage d s | d -> s

--------------------------------------------------------------------------------
-- AdjustLabel
-- TODO: eventually I'll have to deal with the cursor position being somewhere in the Adjust
--------------------------------------------------------------------------------

type AdjustLabel d s = AdjustLabel' (DerLabel' d) (SortLabel s)

type AdjustLabel' = EitherF AdjustLabel''

data AdjustLabel'' s = AdjustBoundary AdjustBoundaryDirection (Tree (ChangeLabel s))

data AdjustBoundaryDirection = Up | Down

downAdjustBoundary :: forall d s. Tree (ChangeLabel s) -> Tree (AdjustLabel' d s) -> Tree (AdjustLabel' d s)
downAdjustBoundary ch kid = LeftF (AdjustBoundary Down ch) %* [ kid ]

upAdjustBoundary :: forall d s. Tree (ChangeLabel s) -> Tree (AdjustLabel' d s) -> Tree (AdjustLabel' d s)
upAdjustBoundary ch kid = LeftF (AdjustBoundary Up ch) %* [ kid ]

infix 1 downAdjustBoundary as ↓
infix 1 upAdjustBoundary as ↑

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

class (IsLanguage d s, HasAdjustRules d s) <= IsAdjustLanguage d s | d -> s

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
   . IsSortRuleLabel s
  => FromObjectToRecord (Tree (ChangeLabel (SortLabel s))) xs
  => Tree (Matchial xs (Tree (ChangeLabel (SortLabel s))) (ChangeLabel (SortLabel s)))
  -> (Record xs -> a)
  -> Tree (ChangeLabel (SortLabel s))
  -> Maybe a
matchTreeChangeSort t1_ k t2_ =
  go t1_ t2_
    # flip execStateT (Object.empty :: Object (Tree (ChangeLabel (SortLabel s))))
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
  => Cons x (Tree (ChangeLabel (SortLabel s))) xs_ xs
  => Proxy x
  -> Tree (Matchial xs (Tree (ChangeLabel (SortLabel s))) a)
matchialVar x = Left (mkExistsProxyCons x) %* []

mkMatchialCongruenceSort :: forall s xs a. s -> Array (Tree (Matchial xs a (ChangeLabel (SortLabel s)))) -> Tree (Matchial xs a (ChangeLabel (SortLabel s)))
mkMatchialCongruenceSort s kids = Right (Congruence (SortLabel s)) % List.fromFoldable kids

infix 1 mkMatchialCongruenceSort as %|∂.^

mkMatchialPlusSort :: forall s xs a f1 f2. Foldable f1 => Foldable f2 => s -> f1 (Tree (SortLabel s)) -> Tree (Matchial xs a (ChangeLabel (SortLabel s))) -> f2 (Tree (SortLabel s)) -> Tree (Matchial xs a (ChangeLabel (SortLabel s)))
mkMatchialPlusSort s kids_left kid kids_right = Right (Plus (Tooth (SortLabel s) (RevList.fromList (List.fromFoldable kids_left)) (List.fromFoldable kids_right))) %* [ kid ]

infixl 1 mkMatchialPlusSort as %|∂+^

mkMatchialMinusSort :: forall s xs a f1 f2. Foldable f1 => Foldable f2 => s -> f1 (Tree (SortLabel s)) -> Tree (Matchial xs a (ChangeLabel (SortLabel s))) -> f2 (Tree (SortLabel s)) -> Tree (Matchial xs a (ChangeLabel (SortLabel s)))
mkMatchialMinusSort s kids_left kid kids_right = Right (Minus (Tooth (SortLabel s) (RevList.fromList (List.fromFoldable kids_left)) (List.fromFoldable kids_right))) %* [ kid ]

infixl 1 mkMatchialMinusSort as %|∂-^

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

derive instance Generic (DerLabel' d s) _

instance (Show s, Show d) => Show (DerLabel' d s) where
  show x = genericShow x

instance (PrettyTreeLabel s, Pretty d) => Pretty (DerLabel' d s) where
  pretty (DerLabel d sigma) = parens $ pretty d <> " " <> pretty sigma
  pretty (DerBoundary ch) = parens $ "!! " <> pretty ch

instance (PrettyTreeLabel s, PrettyTreeLabel d) => PrettyTreeLabel (DerLabel' d s) where
  prettyTree (DerLabel d _sigma) kids = prettyTree d kids
  prettyTree (DerBoundary ch) (kid : Nil) = parens $ pretty ch <> " !! " <> kid
  prettyTree _ _ = bug "invalid `Tree (DerLabel' d s)`"

instance (Eq s, Eq d) => Eq (DerLabel' d s) where
  eq x = genericEq x

derive instance Functor (DerLabel' d)

derive instance Generic (DerivRule s) _

derive instance Newtype (DerivRule s) _

derive instance Generic (DerAdjustRule s) _

derive instance Newtype (DerAdjustRule s) _

derive instance Generic (AdjustLabel'' s) _

instance Show s => Show (AdjustLabel'' s) where
  show x = genericShow x

instance PrettyTreeLabel s => Pretty (AdjustLabel'' s) where
  pretty (AdjustBoundary dir ch) = parens $ "!! " <> pretty dir <> " " <> pretty ch

instance PrettyTreeLabel s => PrettyTreeLabel (AdjustLabel'' s) where
  prettyTree (AdjustBoundary dir ch) (kid : Nil) = parens $ pretty ch <> " " <> pretty dir <> "  " <> kid
  prettyTree _ _ = bug "invalid `AdjustLabel' s`"

instance Eq s => Eq (AdjustLabel'' s) where
  eq x = genericEq x

derive instance Functor AdjustLabel''

derive instance Generic AdjustBoundaryDirection _

instance Show AdjustBoundaryDirection where
  show x = genericShow x

instance Pretty AdjustBoundaryDirection where
  pretty Up = "↑"
  pretty Down = "↓"

instance Eq AdjustBoundaryDirection where
  eq x = genericEq x

derive instance Generic (AdjustRule d s) _

derive instance Newtype (AdjustRule d s) _


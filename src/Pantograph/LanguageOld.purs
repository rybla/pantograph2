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
-- DerivLabel
--------------------------------------------------------------------------------

class (Show d, Eq d, Pretty d, PrettyTreeLabel d) <= IsDerivRuleLabel d

type DerivLabel d s = DerivLabel' d (SortLabel s)

data DerivLabel' d s
  = DerivLabel d (RulialVarSubst (Tree s))
  | DerivBoundary (Tree (ChangeLabel s))

--------------------------------------------------------------------------------
-- DerivRule
--------------------------------------------------------------------------------

newtype DerivRule s = DerivRule
  { sort :: Tree (Rulial (SortLabel s))
  , kids :: List { sort :: Tree (Rulial (SortLabel s)) }
  }

type DerivRules d s = d -> DerivRule s

class HasDerivRules d s | d -> s where
  derivRules :: DerivRules d s

class (IsDerivRuleLabel d, IsSortRuleLabel s, HasDerivRules d s) <= IsLanguage d s | d -> s

--------------------------------------------------------------------------------
-- DerivChangeRule 
--------------------------------------------------------------------------------

newtype DerivChangeRule s = DerivChangeRule
  { kids :: List { change :: Tree (ChangeLabel (SortLabel s)) } }

type DerivChangeRules d s = d -> DerivChangeRule s

class HasDerivChangeRules d s | d -> s where
  derivChangeRules :: DerivChangeRules d s

class (IsLanguage d s, HasDerivChangeRules d s) <= IsDerivChangeLanguage d s | d -> s

--------------------------------------------------------------------------------
-- DerivPropagRule
--------------------------------------------------------------------------------

newtype DerivPropagRule s = DerivPropagRule
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

type DerivPropagRules d s = d -> DerivPropagRule s

class HasDerivPropagRules d s | d -> s where
  derivPropagRules :: DerivPropagRules d s

class (IsLanguage d s, HasDerivPropagRules d s) <= IsDerivPropagLanguage d s | d -> s

--------------------------------------------------------------------------------
-- PropagLabel
-- TODO: eventually I'll have to deal with the cursor position being somewhere in the Propag
--------------------------------------------------------------------------------

type PropagLabel d s = PropagLabel' (DerivLabel' d) (SortLabel s)

type PropagLabel' = EitherF PropagLabel''

data PropagLabel'' s = PropagBoundary PropagBoundaryDirection (Tree (ChangeLabel s))

data PropagBoundaryDirection = Up | Down

downPropagBoundary :: forall d s. Tree (ChangeLabel s) -> Tree (PropagLabel' d s) -> Tree (PropagLabel' d s)
downPropagBoundary ch kid = LeftF (PropagBoundary Down ch) %* [ kid ]

upPropagBoundary :: forall d s. Tree (ChangeLabel s) -> Tree (PropagLabel' d s) -> Tree (PropagLabel' d s)
upPropagBoundary ch kid = LeftF (PropagBoundary Up ch) %* [ kid ]

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

class (IsLanguage d s, HasPropagRules d s) <= IsPropagLanguage d s | d -> s

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

class (IsPropagLanguage d s, HasInsertRules d s) <= IsInsertLanguage d s | d -> s

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

derive instance Generic (DerivPropagRule s) _

derive instance Newtype (DerivPropagRule s) _

derive instance Generic (PropagLabel'' s) _

instance Show s => Show (PropagLabel'' s) where
  show x = genericShow x

instance PrettyTreeLabel s => Pretty (PropagLabel'' s) where
  pretty (PropagBoundary dir ch) = parens $ "!! " <> pretty dir <> " " <> pretty ch

instance PrettyTreeLabel s => PrettyTreeLabel (PropagLabel'' s) where
  prettyTree (PropagBoundary dir ch) (kid : Nil) = parens $ pretty ch <> " " <> pretty dir <> "  " <> kid
  prettyTree _ _ = bug "invalid `PropagLabel' s`"

instance Eq s => Eq (PropagLabel'' s) where
  eq x = genericEq x

derive instance Functor PropagLabel''

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


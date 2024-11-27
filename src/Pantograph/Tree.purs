module Pantograph.Tree where

import Prelude

import Control.Alternative (guard)
import Control.MonadPlus (class Plus, empty)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, fold, length)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe, fromMaybe')
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol)
import Data.Traversable (class Traversable, foldl)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as V
import Debug as Debug
import Pantograph.Pretty (class Pretty, braces, parens, pretty)
import Pantograph.RevList (RevList)
import Pantograph.RevList as RevList
import Pantograph.Utility (bug, expand1', todo)
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- Tree
--------------------------------------------------------------------------------

data Tree a = Tree a (List (Tree a))
type TreeV l = Tree (Variant l)

derive instance Generic (Tree a) _

instance Show a => Show (Tree a) where
  show x = genericShow x

instance Eq a => Eq (Tree a) where
  eq x = genericEq x

instance PrettyTreeL l => Pretty (Tree l) where
  pretty (l %% kids) = prettyTreeL l (pretty <$> kids)

derive instance Functor Tree
derive instance Foldable Tree
derive instance Traversable Tree

instance Apply Tree where
  apply (Tree f tfs) tx@(Tree x txs) = Tree (f x) (map (map f) txs <> map (_ <*> tx) tfs)

instance Applicative Tree where
  pure x = Tree x mempty

instance Bind Tree where
  bind (Tree x ts) f = let Tree x' ts' = f x in Tree x' (ts' <> map (_ >>= f) ts)

instance Monad Tree

infixl 3 Tree as %%

makeTree :: forall a f. Foldable f => a -> f (Tree a) -> Tree a
makeTree a = Tree a <<< List.fromFoldable

infixl 3 makeTree as %

class PrettyTreeL a where
  prettyTreeL :: a -> List String -> String

instance PrettyTreeL_R l => PrettyTreeL (Variant l) where
  prettyTreeL = prettyTreeL_Row (Proxy @l)

class PrettyTreeL_R (l :: Row Type) where
  prettyTreeL_Row :: Proxy l -> Variant l -> List String -> String

instance (RowToList l rl, PrettyTreeL_RL l rl) => PrettyTreeL_R l where
  prettyTreeL_Row p_l = prettyTreeL_RowList p_l (Proxy @rl)

class PrettyTreeL_RL (l :: Row Type) (rl :: RowList Type) | rl -> l where
  prettyTreeL_RowList :: Proxy l -> Proxy rl -> Variant l -> List String -> String

instance
  ( Cons x a l l'
  , IsSymbol x
  , PrettyTreeL a
  , PrettyTreeL_RL l rl
  ) =>
  PrettyTreeL_RL l' (RowList.Cons x a rl) where
  prettyTreeL_RowList _ _ =
    V.on (Proxy @x) prettyTreeL
      $ prettyTreeL_RowList (Proxy @l) (Proxy @rl)

instance PrettyTreeL_RL () RowList.Nil where
  prettyTreeL_RowList _ _ = V.case_

--------------------------------------------------------------------------------
-- Tooth
--------------------------------------------------------------------------------

data Tooth a = Tooth a (RevList (Tree a)) (List (Tree a))
type ToothV l = Tooth (Variant l)

derive instance Generic (Tooth a) _

instance Show a => Show (Tooth a) where
  show x = genericShow x

instance PrettyTreeL a => Pretty (Tooth a) where
  pretty th = prettyToothS th "{ }"

prettyToothS :: forall a. PrettyTreeL a => Tooth a -> String -> String
prettyToothS (Tooth a kids_l kids_r) str = prettyTreeL a ((kids_l # map pretty # List.fromFoldable) <> ("{ " <> str <> " }") : (kids_r # map pretty))

instance Eq a => Eq (Tooth a) where
  eq x = genericEq x

derive instance Functor Tooth
derive instance Foldable Tooth
derive instance Traversable Tooth

unTooth :: forall a. Tooth a -> Tree a -> Tree a
unTooth (Tooth a kids_left kids_right) kid_middle = Tree a (RevList.toList kids_left <> kid_middle : kids_right)

getTeeth :: forall a. Tree a -> List (Tooth a /\ Tree a)
getTeeth (a %% kids) = go mempty mempty kids
  where
  go :: List (Tooth a /\ Tree a) -> RevList (Tree a) -> List (Tree a) -> List (Tooth a /\ Tree a)
  go results _ls Nil = results
  go results ls (t : rs) = go ((Tooth a ls rs /\ t) : results) (ls `RevList.snoc` t) rs

--------------------------------------------------------------------------------
-- Path
--------------------------------------------------------------------------------

data Path a = Path (List (Tooth a))
type PathV l = Path (Variant l)

derive instance Generic (Path a) _

instance Show a => Show (Path a) where
  show x = genericShow x

instance PrettyTreeL a => Pretty (Path a) where
  pretty (Path ths) = ths # foldl (\str th -> prettyToothS th str) "{ }"

instance Eq a => Eq (Path a) where
  eq x = genericEq x

instance Semigroup (Path a) where
  append (Path ths1) (Path ths2) = Path (ths1 <> ths2)

instance Monoid (Path a) where
  mempty = Path mempty

stepPath :: forall a. Path a -> Tooth a -> Path a
stepPath (Path ths) th = Path (th : ths)

unstepPath :: forall f a. Plus f => Applicative f => Path a -> f (Path a /\ Tooth a)
unstepPath (Path Nil) = empty
unstepPath (Path (th : ths)) = pure (Path ths /\ th)

unPath :: forall a. Path a -> Tree a -> Tree a
unPath (Path Nil) t = t
unPath (Path (th : ths)) t = unPath (Path ths) (unTooth th t)

--------------------------------------------------------------------------------
-- Change
--------------------------------------------------------------------------------

type ChangeR r =
  ( plus :: PlusChange r
  , minus :: MinusChange r
  , replace :: ReplaceChange r
  | r
  )

expand_ChangeR :: forall r. Variant r -> Variant (ChangeR r)
expand_ChangeR = expand1' @"plus" >>> expand1' @"minus" >>> expand1' @"replace"

data PlusChange l = PlusChange (Tooth (Variant l))

derive instance Generic (PlusChange l) _

instance Show (Variant l) => Show (PlusChange l) where
  show x = genericShow x

derive instance Eq (Variant l) => Eq (PlusChange l)

instance PrettyTreeL_R l => PrettyTreeL (PlusChange l) where
  prettyTreeL (PlusChange (Tooth a ls rs)) (kid : Nil) =
    "+" <>
      ( braces
          $ prettyTreeL a
          $ fold
              [ ls # map pretty # RevList.toList
              , pure $ braces kid
              , rs # map pretty
              ]
      )
  prettyTreeL _ _ = bug "invalid "

data MinusChange l = MinusChange (Tooth (Variant l))

derive instance Generic (MinusChange l) _

instance Show (Variant l) => Show (MinusChange l) where
  show x = genericShow x

derive instance Eq (Variant l) => Eq (MinusChange l)

instance PrettyTreeL_R l => PrettyTreeL (MinusChange l) where
  prettyTreeL (MinusChange (Tooth a ls rs)) (kid : Nil) =
    "-" <>
      ( braces $ prettyTreeL a
          $ fold
              [ ls # map pretty # RevList.toList
              , pure $ braces kid
              , rs # map pretty
              ]
      )
  prettyTreeL _ _ = bug "invalid "

data ReplaceChange l = ReplaceChange (Tree (Variant l)) (Tree (Variant l))

derive instance Generic (ReplaceChange l) _

instance Show (Variant l) => Show (ReplaceChange l) where
  show x = genericShow x

derive instance Eq (Variant l) => Eq (ReplaceChange l)

instance PrettyTreeL_R l => PrettyTreeL (ReplaceChange l) where
  prettyTreeL (ReplaceChange t t') Nil =
    parens (pretty t) <> " ~~> " <> parens (pretty t')
  prettyTreeL _ _ = bug "invalid "

id :: forall l l_. Union l l_ (ChangeR l) => TreeV l -> TreeV (ChangeR l)
id = map V.expand

composeChanges :: forall l. TreeV (ChangeR l) -> TreeV (ChangeR l) -> Maybe (TreeV (ChangeR l))
composeChanges _ _ = todo "composeChanges"

invertChange :: forall l. TreeV (ChangeR l) -> TreeV (ChangeR l)
invertChange _ = todo "invertChange"

innerEndpoint :: forall l. Eq (Variant l) => TreeV (ChangeR l) -> TreeV l
innerEndpoint (l %% kids) =
  V.case_
    # (\_ l' -> l' %% (kids <#> innerEndpoint))
    # V.on (Proxy @"plus")
        ( \(PlusChange _th) -> case kids of
            -- c : Nil -> c # innerEndpoint # minusTooth th # fromMaybe' \_ -> bug "invalid change since can't minusTooth when getting inner endpoint of a PlusChange"
            c : Nil -> c # innerEndpoint
            _ -> bug "invalid Change"
        )
    # V.on (Proxy @"minus")
        ( \(MinusChange th) -> case kids of
            c : Nil -> unTooth th (c # innerEndpoint)
            _ -> bug "invalid Change"
        )
    # V.on (Proxy @"replace") (\(ReplaceChange t0 _t1) -> t0)
    $ l

_inner = innerEndpoint

outerEndpoint :: forall l. Eq (Variant l) => TreeV (ChangeR l) -> TreeV l
outerEndpoint (l %% kids) =
  V.case_
    # (\_ l' -> l' %% (kids <#> outerEndpoint))
    # V.on (Proxy @"plus")
        ( \(PlusChange th) -> case kids of
            c : Nil -> unTooth th (c # outerEndpoint)
            _ -> bug "invalid Change"
        )
    # V.on (Proxy @"minus")
        ( \(MinusChange _th) -> case kids of
            -- c : Nil -> c # outerEndpoint # minusTooth th # fromMaybe' \_ -> bug "invalid change since can't minusTooth when getting outer endpoint of a MinusChange"
            c : Nil -> c # outerEndpoint
            _ -> bug "invalid Change"
        )
    # V.on (Proxy @"replace") (\(ReplaceChange _t0 t1) -> t1)
    $ l

_outer = outerEndpoint

minusTooth :: forall l. Eq (Variant l) => ToothV l -> TreeV l -> Maybe (TreeV l)
minusTooth (Tooth l ls rs) (l' %% ts) = do
  guard $ l == l'
  if not ((ls # length) < ((ts # length) - 1 :: Int)) then bug "invalid label since wrong number of kids" else pure unit
  guard $ List.zip (ls # List.fromFoldable) (ts # List.take (ls # length)) # List.all (uncurry eq)
  if not ((rs # length) < ((ts # length) - 1 :: Int)) then bug "invalid label since wrong number of kids" else pure unit
  guard $ List.zip rs (ts # List.takeEnd (rs # length)) # List.all (uncurry eq)
  let t = ts List.!! (ls # length) # fromMaybe' \_ -> bug "impossible"
  pure t

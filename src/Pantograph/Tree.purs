module Pantograph.Tree where

import Prelude

import Control.MonadPlus (class Plus, empty)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, fold, foldl, intercalate, length)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Data.Tuple.Nested (type (/\), (/\))
import Pantograph.Pretty (class Pretty, parens, pretty)
import Pantograph.RevList (RevList)
import Pantograph.RevList as RevList
import Pantograph.Utility (bug)

--------------------------------------------------------------------------------
-- Tree
--------------------------------------------------------------------------------

data Tree a = Tree a (List (Tree a))

infix 0 Tree as ▵

mkTree :: forall a f. Foldable f => a -> f (Tree a) -> Tree a
mkTree a = Tree a <<< List.fromFoldable

infix 0 mkTree as ▵*

derive instance Generic (Tree a) _

instance Show a => Show (Tree a) where
  show x = genericShow x

instance Eq a => Eq (Tree a) where
  eq x = genericEq x

instance Pretty a => Pretty (Tree a) where
  pretty (a ▵ Nil) = parens $ pretty a
  pretty (a ▵ kids) = parens $ pretty a <> " ▵ " <> (kids # map pretty # intercalate " ")

derive instance Functor Tree
derive instance Foldable Tree
derive instance Traversable Tree

getTeeth :: forall a. Tree a -> List (Tooth a /\ Tree a)
getTeeth (Tree a ts) = go mempty mempty ts # RevList.toList
  where
  go ths _ Nil = ths
  go ths ts_l (t : ts_r) = go (ths `RevList.snoc` (Tooth a ts_l ts_r /\ t)) (ts_l `RevList.snoc` t) ts_r

--------------------------------------------------------------------------------
-- Tooth
--------------------------------------------------------------------------------

data Tooth a = Tooth a (RevList (Tree a)) (List (Tree a))

mkTooth :: forall a f1 f2. Foldable f1 => Foldable f2 => a -> f1 (Tree a) /\ f2 (Tree a) -> Tooth a
mkTooth a (l /\ r) = Tooth a (RevList.fromFoldable l) (List.fromFoldable r)

infix 0 mkTooth as ▵<

derive instance Generic (Tooth a) _

instance Show a => Show (Tooth a) where
  show x = genericShow x

instance Pretty a => Pretty (Tooth a) where
  pretty th = prettyToothS th "{}"

prettyToothS :: forall a. Pretty a => Tooth a -> String -> String
prettyToothS (Tooth a kids_l kids_r) str = parens $ pretty a <> " ▵ " <>
  ( [ kids_l # map pretty # List.fromFoldable
    , str # pure
    , kids_r # map pretty
    ]
      # fold >>> intercalate " "
  )

instance Eq a => Eq (Tooth a) where
  eq x = genericEq x

derive instance Functor Tooth
derive instance Foldable Tooth
derive instance Traversable Tooth

unTooth :: forall a. Tooth a -> Tree a -> Tree a
unTooth (Tooth a kids_left kids_right) kid_middle = Tree a (RevList.toList kids_left <> kid_middle : kids_right)

--------------------------------------------------------------------------------
-- Path
--------------------------------------------------------------------------------

data Path a = Path (List (Tooth a))

derive instance Generic (Path a) _

instance Show a => Show (Path a) where
  show x = genericShow x

instance Pretty a => Pretty (Path a) where
  pretty (Path ths) = ths # foldl (\str th -> prettyToothS th str) "{}"

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

--------------------------------------------------------------------------------
-- Change
--------------------------------------------------------------------------------

data ChangeLabel l
  = Congruence l
  | Plus (Tooth l)
  | Minus (Tooth l)
  | Replace (Tree l) (Tree l)

type Change l = Tree (ChangeLabel l)

mkCongruence :: forall f l. Foldable f => l -> f (Change l) -> Change l
mkCongruence l kids = Congruence l ▵* kids

infix 0 mkCongruence as ▵∂.

id :: forall l. Tree l -> Change l
id = map Congruence

mkPlus :: forall l. Tooth l -> Change l -> Change l
mkPlus l kid = Plus l ▵* [ kid ]

infix 0 mkPlus as ▵∂+

mkMinus :: forall l. Tooth l -> Change l -> Change l
mkMinus l kid = Minus l ▵* [ kid ]

infix 0 mkMinus as ▵∂-

mkReplace :: forall l. Tree l -> Tree l -> Change l
mkReplace t1 t2 = Replace t1 t2 ▵* []

infix 0 mkReplace as ▵∂~>

derive instance Generic (ChangeLabel l) _

instance Show l => Show (ChangeLabel l) where
  show x = genericShow x

instance Pretty l => Pretty (ChangeLabel l) where
  pretty (Congruence l) = pretty l
  pretty (Plus th) = "+ " <> pretty th
  pretty (Minus th) = "- " <> pretty th
  pretty (Replace x y) = pretty x <> " ~~> " <> pretty y

instance Eq l => Eq (ChangeLabel l) where
  eq x = genericEq x

derive instance Functor ChangeLabel
derive instance Foldable ChangeLabel
derive instance Traversable ChangeLabel

leftEndpoint :: forall l. Change l -> Tree l
leftEndpoint (Congruence l ▵ cs) = Tree l (leftEndpoint <$> cs)
leftEndpoint (Plus _ ▵ (c : Nil)) = leftEndpoint c
leftEndpoint (Plus _ ▵ _) = bug "Tree with Plus label should have exactly 1 kid"
leftEndpoint (Minus th ▵ (c : Nil)) = unTooth th (leftEndpoint c)
leftEndpoint (Minus _ ▵ _) = bug "Tree with Minus label should have exactly 1 kid"
leftEndpoint (Replace t _ ▵ Nil) = t
leftEndpoint (Replace _ _ ▵ _) = bug "Tree with Replace label should have exactly 0 kids"

rightEndpoint :: forall l. Change l -> Tree l
rightEndpoint (Congruence l ▵ cs) = Tree l (rightEndpoint <$> cs)
rightEndpoint (Plus th ▵ (c : Nil)) = unTooth th (rightEndpoint c)
rightEndpoint (Plus _ ▵ _) = bug "Tree with Plus label should have exactly 1 kid"
rightEndpoint (Minus _ ▵ (c : Nil)) = rightEndpoint c
rightEndpoint (Minus _ ▵ _) = bug "Tree with Minus label should have exactly 1 kid"
rightEndpoint (Replace t _ ▵ Nil) = t
rightEndpoint (Replace _ _ ▵ _) = bug "Tree with Replace label should have exactly 0 kids"

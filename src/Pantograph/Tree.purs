module Pantograph.Tree where

import Prelude

import Control.Alternative (guard)
import Control.MonadPlus (class Plus, empty)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, fold, foldl, intercalate, length)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe, fromMaybe')
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Pantograph.EitherF (EitherF(..))
import Pantograph.Pretty (class Pretty, parens, pretty)
import Pantograph.RevList (RevList)
import Pantograph.RevList as RevList
import Pantograph.Utility (bug, todo)

--------------------------------------------------------------------------------
-- Tree
--------------------------------------------------------------------------------

data Tree a = Tree a (List (Tree a))

infix 1 Tree as %

mkTree :: forall a f. Foldable f => a -> f (Tree a) -> Tree a
mkTree a = Tree a <<< List.fromFoldable

infix 1 mkTree as %*

derive instance Generic (Tree a) _

instance Show a => Show (Tree a) where
  show x = genericShow x

instance Eq a => Eq (Tree a) where
  eq x = genericEq x

class PrettyTreeLabel a where
  prettyTree :: a -> List String -> String

instance (PrettyTreeLabel a, PrettyTreeLabel b) => PrettyTreeLabel (Either a b) where
  prettyTree (Left a) = prettyTree a
  prettyTree (Right b) = prettyTree b

instance (PrettyTreeLabel (a x), PrettyTreeLabel (b x)) => PrettyTreeLabel (EitherF a b x) where
  prettyTree (LeftF a) = prettyTree a
  prettyTree (RightF b) = prettyTree b

instance PrettyTreeLabel a => Pretty (Tree a) where
  pretty (a % kids) = prettyTree a (kids # map pretty)

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

infix 1 mkTooth as %<

derive instance Generic (Tooth a) _

instance Show a => Show (Tooth a) where
  show x = genericShow x

instance PrettyTreeLabel a => Pretty (Tooth a) where
  pretty th = prettyToothS th "{}"

prettyToothS :: forall a. PrettyTreeLabel a => Tooth a -> String -> String
prettyToothS (Tooth a kids_l kids_r) str = prettyTree a ((kids_l # map pretty # List.fromFoldable) <> "{}" : (kids_r # map pretty))

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

instance PrettyTreeLabel a => Pretty (Path a) where
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

isValidChange :: forall l. Tree (ChangeLabel l) -> Boolean
isValidChange (Congruence _ % _) = true -- TODO: could take into account DerivRules, but then need to refactor modules to move this and compose into Grammar, which I probabably don't want to do afterall
isValidChange (Plus _ % (_ : Nil)) = true
isValidChange (Minus _ % (_ : Nil)) = true
isValidChange (Replace _ _ % Nil) = true
isValidChange _ = false

mkCongruence :: forall f l. Foldable f => l -> f (Tree (ChangeLabel l)) -> Tree (ChangeLabel l)
mkCongruence l kids = Congruence l %* kids

infix 1 mkCongruence as %∂.

id :: forall l. Tree l -> Tree (ChangeLabel l)
id = map Congruence

id' :: forall f l. Functor f => Tree (f l) -> Tree (f (ChangeLabel l))
id' = map (map Congruence)

mkPlus :: forall l. Tooth l -> Tree (ChangeLabel l) -> Tree (ChangeLabel l)
mkPlus l kid = Plus l %* [ kid ]

infix 1 mkPlus as %∂+

mkMinus :: forall l. Tooth l -> Tree (ChangeLabel l) -> Tree (ChangeLabel l)
mkMinus l kid = Minus l %* [ kid ]

infix 1 mkMinus as %∂-

mkReplace :: forall l. Tree l -> Tree l -> Tree (ChangeLabel l)
mkReplace t1 t2 = Replace t1 t2 %* []

infix 1 mkReplace as %∂~>

derive instance Generic (ChangeLabel l) _

instance Show l => Show (ChangeLabel l) where
  show x = genericShow x

instance PrettyTreeLabel l => PrettyTreeLabel (ChangeLabel l) where
  prettyTree (Congruence l) kids = prettyTree l kids
  prettyTree (Plus th) (kid : Nil) = "+ ⟨ " <> prettyToothS th (" ⟨ " <> kid <> " ⟩ ") <> " ⟩ "
  prettyTree (Minus th) (kid : Nil) = "- ⟨ " <> prettyToothS th (" ⟨ " <> kid <> " ⟩ ") <> " ⟩ "
  prettyTree (Replace x y) Nil = pretty x <> " ~> " <> pretty y
  prettyTree _ _ = bug "invlalid `Tree (ChangeLabel l)`"

instance Eq l => Eq (ChangeLabel l) where
  eq x = genericEq x

derive instance Functor ChangeLabel
derive instance Foldable ChangeLabel
derive instance Traversable ChangeLabel

composeChanges' :: forall f l. Applicative f => Traversable f => Eq l => Tree (f (ChangeLabel l)) -> Tree (f (ChangeLabel l)) -> Maybe (Tree (f (ChangeLabel l)))
composeChanges' _ _ = todo "composeChanges'"

composeChanges :: forall l. Eq l => Tree (ChangeLabel l) -> Tree (ChangeLabel l) -> Maybe (Tree (ChangeLabel l))

composeChanges c1 c2 | not (isValidChange c1 && isValidChange c2) = bug "invalid Change"

composeChanges (Congruence l1 % cs1) (Congruence l2 % cs2) | l1 == l2 = do
  cs <- List.zip cs1 cs2 # traverse (uncurry composeChanges)
  pure $ Congruence l1 % cs

composeChanges (Congruence l1 % cs1) (Plus (Tooth l2 l r) % (c2 : Nil)) = do
  guard $ l1 == l2
  l' <-
    traverse (uncurry composeChanges) $
      RevList.zip
        (cs1 # List.take (length l) # RevList.fromList)
        (l # map id)
  c <-
    composeChanges
      (cs1 List.!! (length l + 1) # fromMaybe' \_ -> bug "cs1 isn't long enough")
      c2
  r' <-
    traverse (uncurry composeChanges) $
      List.zip
        (cs1 # List.takeEnd (length r))
        (r # map id)
  pure $ Plus (Tooth l2 (l' # map outerEndpoint) (r' # map outerEndpoint)) % (c : Nil)

composeChanges (Minus (Tooth l1 l r) % (c1 : Nil)) (Congruence l2 % cs2) = do
  guard $ l1 == l2
  l' <-
    traverse (uncurry composeChanges) $
      RevList.zip
        (l # map id)
        (cs2 # List.take (length l) # RevList.fromList)
  c <-
    composeChanges
      c1
      (cs2 List.!! length l # fromMaybe' \_ -> bug "cs2 isn't long enough")
  r' <-
    traverse (uncurry composeChanges) $
      List.zip
        (r # map id)
        (cs2 # List.takeEnd (length r))
  pure $ Minus (Tooth l2 (l' # map outerEndpoint) (r' # map outerEndpoint)) % (c : Nil)

composeChanges c1_@(Minus (Tooth lbl1 l1 r1) % (c1 : Nil)) c2_@(Plus (Tooth lbl2 l2 r2) % (c2 : Nil)) = do
  guard $ lbl1 == lbl2
  if l1 == l2 && r1 == r2 then do
    c <- composeChanges c1 c2
    pure $ Congruence lbl1 % (c : Nil)
  else do
    guard $ (c2_ # outerEndpoint) == (c1_ # innerEndpoint)
    let t = c2_ # innerEndpoint
    let t' = c1_ # outerEndpoint
    pure $ Replace t t' % Nil

composeChanges c1 (Minus th2 % (c2 : Nil)) = do
  c <- composeChanges c1 c2
  pure $ Minus th2 % (c : Nil)

composeChanges (Plus th % (c1 : Nil)) c2 = do
  c <- composeChanges c1 c2
  pure $ Plus th % (c : Nil)

composeChanges c1 (Replace t2 t2' % Nil) = do
  c <- composeChanges c1 (t2' # id)
  pure $ Replace t2 (c # outerEndpoint) % Nil

composeChanges (Replace t1 t1' % Nil) c2 = do
  c <- composeChanges (t1 # id) c2
  pure $ Replace (c # innerEndpoint) t1' % Nil

composeChanges _ _ = empty

invertChange :: forall l. Tree (ChangeLabel l) -> Tree (ChangeLabel l)
invertChange c | not (isValidChange c) = bug "invalid Change"
invertChange (Congruence l % cs) = Congruence l % (cs # map invertChange)
invertChange (Plus th % (c : Nil)) = Minus th % ((c # invertChange) : Nil)
invertChange (Minus th % (c : Nil)) = Plus th % ((c # invertChange) : Nil)
invertChange (Replace t t' % Nil) = Replace t' t % Nil
invertChange _ = bug "impossible"

innerEndpoint :: forall l. Tree (ChangeLabel l) -> Tree l
innerEndpoint c | not (isValidChange c) = bug "invalid Change"
innerEndpoint (Congruence l % cs) = Tree l (innerEndpoint <$> cs)
innerEndpoint (Plus _ % (c : Nil)) = innerEndpoint c
innerEndpoint (Minus th % (c : Nil)) = unTooth th (innerEndpoint c)
innerEndpoint (Replace t _ % Nil) = t
innerEndpoint (_ % _) = bug "impossible"

innerEndpoint' :: forall f l. Applicative f => Traversable f => Tree (f (ChangeLabel l)) -> Tree (f l)
innerEndpoint' = sequence >>> map innerEndpoint >>> sequence

outerEndpoint :: forall l. Tree (ChangeLabel l) -> Tree l
outerEndpoint c | not (isValidChange c) = bug "invalid Change"
outerEndpoint (Congruence l % cs) = Tree l (outerEndpoint <$> cs)
outerEndpoint (Plus th % (c : Nil)) = unTooth th (outerEndpoint c)
outerEndpoint (Minus _ % (c : Nil)) = outerEndpoint c
outerEndpoint (Replace t _ % Nil) = t
outerEndpoint (_ % _) = bug "impossible"

outerEndpoint' :: forall f l. Applicative f => Traversable f => Tree (f (ChangeLabel l)) -> Tree (f l)
outerEndpoint' = sequence >>> map outerEndpoint >>> sequence

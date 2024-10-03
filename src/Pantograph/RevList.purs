module Pantograph.RevList where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)

data RevList a = RevList (List a)

derive instance Generic (RevList a) _

instance Show a => Show (RevList a) where
  show x = genericShow x

instance Eq a => Eq (RevList a) where
  eq x = genericEq x

derive instance Functor RevList
derive instance Foldable RevList
derive instance Traversable RevList

instance Semigroup (RevList a) where
  append (RevList xs) (RevList ys) = RevList (xs <> ys)

instance Monoid (RevList a) where
  mempty = RevList mempty

fromList :: List ~> RevList
fromList = List.reverse >>> RevList

toList :: RevList ~> List
toList (RevList xs) = xs # List.reverse

snoc :: forall a. RevList a -> a -> RevList a
snoc (RevList xs) x = (RevList (Cons x xs))

unsnoc :: forall a. RevList a -> Maybe { init :: RevList a, last :: a }
unsnoc (RevList xs) = List.uncons xs <#> \{ head, tail } -> { init: RevList tail, last: head }


module Pantograph.EitherF where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)

data EitherF l r a = LeftF (l a) | RightF (r a)

derive instance Generic (EitherF l r a) _

instance (Show (l a), Show (r a)) => Show (EitherF l r a) where
  show x = genericShow x

instance (Eq (l a), Eq (r a), Eq a) => Eq (EitherF l r a) where
  eq x = genericEq x

instance (Ord (l a), Ord (r a), Ord a) => Ord (EitherF l r a) where
  compare x = genericCompare x

instance (Functor l, Functor r) => Functor (EitherF l r) where
  map f (LeftF ma) = LeftF (map f ma)
  map f (RightF ma) = RightF (map f ma)

-- instance (Applicative l, Applicative r) => Apply (EitherF l r) where
--   apply (LeftF mf) (LeftF ma) = LeftF (apply mf ma)
--   apply (LeftF mf) (RightF a) = LeftF (apply mf (pure a))
--   apply (RightF f) (LeftF ma) = LeftF (map f ma)
--   apply (RightF f) (RightF a) = RightF (apply mf (pure a))

-- instance Applicative m => Applicative (EitherF m) where
--   pure = RightF

-- instance Monad m => Bind (EitherF m) where
--   bind (LeftF ma) k = LeftF (ma >>= k >>> eitherF identity pure)
--   bind (RightF a) k = k a

-- instance Monad m => Monad (EitherF m)

eitherF l r = case _ of
  LeftF x -> l x
  RightF x -> r x


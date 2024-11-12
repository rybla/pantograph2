module SuperType where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Pantograph.TypeList (type (:*), TypeList)
import Pantograph.TypeList as TL

--------------------------------------------------------------------------------
-- SuperTypeStep
--------------------------------------------------------------------------------

class SuperTypeStep l1 l2 | l1 -> l2 where
  injectStep :: l2 -> l1

instance SuperTypeStep (Maybe a) a where
  injectStep = pure

instance SuperTypeStep (Either a b) b where
  injectStep = pure

--------------------------------------------------------------------------------
-- SuperTypeChain
--------------------------------------------------------------------------------

class SuperTypeChain a (ts :: TypeList) b | a b -> ts where
  inject :: b -> a

instance SuperTypeChain a TL.Nil a where
  inject = identity
else instance (SuperTypeStep a b, SuperTypeChain b ts c) => SuperTypeChain a (b :* ts) c where
  inject = injectStep <<< inject

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

ex1 :: Maybe Int
ex1 = inject 1

ex2 :: Maybe (Maybe Int)
ex2 = inject 1

ex3 :: Maybe (Maybe (Maybe Int))
ex3 = inject 1

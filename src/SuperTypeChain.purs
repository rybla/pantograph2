module SuperTypeChain where

import Prelude

import Data.Maybe (Maybe)

--------------------------------------------------------------------------------
-- TypeList
--------------------------------------------------------------------------------

foreign import data TypeList :: Type
foreign import data Nil :: TypeList
foreign import data Cons :: Type -> TypeList -> TypeList

infixr 1 type Cons as :*

-- class Head :: TypeList -> Type -> Constraint
-- class Head ts t | ts -> t

-- instance Head (t :* ts) t

-- class Head :: TypeList -> Type -> Type -> Constraint
-- class Head ts t_default t | ts t_default -> t

-- instance Head Nil t_default t_default
-- else instance Head (t :* ts) t_default t

--------------------------------------------------------------------------------
-- SuperTypeStep
--------------------------------------------------------------------------------

class SuperTypeStep a b | a -> b where
  injectStep :: b -> a

instance SuperTypeStep (Maybe a) a where
  injectStep = pure

--------------------------------------------------------------------------------
-- SuperTypeChain
--------------------------------------------------------------------------------

class SuperTypeChain a (ts :: TypeList) b | a b -> ts where
  injectChain :: b -> a

instance SuperTypeChain a Nil a where
  injectChain = identity
else instance (SuperTypeStep a b, SuperTypeChain b ts c) => SuperTypeChain a (b :* ts) c where
  injectChain = injectStep <<< injectChain

ex1 :: Maybe Int
ex1 = injectChain 1

ex2 :: Maybe (Maybe Int)
ex2 = injectChain 1

ex3 :: Maybe (Maybe (Maybe Int))
ex3 = injectChain 1


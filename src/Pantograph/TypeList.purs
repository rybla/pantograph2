module Pantograph.TypeList where

import Prelude

--------------------------------------------------------------------------------
-- TypeList
--------------------------------------------------------------------------------

foreign import data TypeList :: Type
foreign import data Nil :: TypeList
foreign import data Cons :: Type -> TypeList -> TypeList

infixr 1 type Cons as :*

class Head :: TypeList -> Type -> Constraint
class Head ts t | ts -> t

instance Head (t :* ts) t

-- class Head :: TypeList -> Type -> Type -> Constraint
-- class Head ts t_default t | ts t_default -> t

-- instance Head Nil t_default t_default
-- else instance Head (t :* ts) t_default t

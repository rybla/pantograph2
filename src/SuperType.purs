module SuperType where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))

--------------------------------------------------------------------------------
-- SuperType
--------------------------------------------------------------------------------

class SuperType l1 l2 | l1 -> l2 where
  inject :: l2 -> l1

instance SuperType (Maybe a) a where
  inject = pure

instance SuperType (Either a b) b where
  inject = pure

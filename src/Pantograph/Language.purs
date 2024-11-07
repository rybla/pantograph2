module Pantograph.Language where

import Pantograph.Tree
import Prelude

import Control.Plus (empty)
import Data.Bifunctor (lmap)
import Data.Eq.Generic (genericEq)
import Data.Function as Function
import Data.Functor.Variant (VariantF)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe', isNothing, maybe, maybe')
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import MetaVar (MetaVar)
import MetaVar as MetaVar
import Pantograph.Pretty (class Pretty, pretty)
import Pantograph.RevList as RevList
import Pantograph.Utility (bug, todo)
import SuperType (class SuperType, inject)

--------------------------------------------------------------------------------
-- MetaLbl
--------------------------------------------------------------------------------

data MetaLbl l
  = MetaVar MetaVar
  | InjMetaLbl l

instance SuperType l l' => SuperType (MetaLbl l) l' where
  inject = InjMetaLbl <<< inject

--------------------------------------------------------------------------------
-- SortLbl
--------------------------------------------------------------------------------

data SortLbl l = InjSortLbl l

instance SuperType l l' => SuperType (SortLbl l) l' where
  inject = InjSortLbl <<< inject

--------------------------------------------------------------------------------
-- DervLbl
--------------------------------------------------------------------------------

data DervLbl l = DervLbl l (MetaVar.Subst (Tree l))


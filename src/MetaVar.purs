module MetaVar where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe')
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Pantograph.Pretty (class Pretty, pretty)
import Pantograph.Tree (class PrettyTreeL)
import Pantograph.Utility (bug)

--------------------------------------------------------------------------------
-- MetaVar
--------------------------------------------------------------------------------

data MetaVar = MetaVar String

derive instance Generic MetaVar _

instance Show MetaVar where
  show x = genericShow x

instance Pretty MetaVar where
  pretty (MetaVar str) = "$" <> str

instance PrettyTreeL MetaVar where
  prettyTreeL x _ = pretty x

instance Eq MetaVar where
  eq x = genericEq x

instance Ord MetaVar where
  compare x = genericCompare x

type Subst a = Map MetaVar a

getMetaVar :: forall a. MetaVar -> Map MetaVar a -> a
getMetaVar x sigma = Map.lookup x sigma # fromMaybe' \_ ->
  bug $ "unrecognized MetaVar: " <> pretty x

getMetaVarFlipped = flip getMetaVar

infix 3 getMetaVarFlipped as !!


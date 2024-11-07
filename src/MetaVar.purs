module MetaVar where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Pantograph.Pretty (class Pretty)

--------------------------------------------------------------------------------
-- MetaVar
--------------------------------------------------------------------------------

data MetaVar = MetaVar String

derive instance Generic MetaVar _

instance Show MetaVar where
  show x = genericShow x

instance Pretty MetaVar where
  pretty (MetaVar str) = "$" <> str

instance Eq MetaVar where
  eq x = genericEq x

instance Ord MetaVar where
  compare x = genericCompare x

type Subst a = Map MetaVar a

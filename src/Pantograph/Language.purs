module Pantograph.Language where

import Pantograph.Tree
import Prelude

import Control.Alternative (empty)
import Data.Bifunctor (class Bifunctor)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Traversable (class Foldable, class Traversable)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import MetaVar (MetaVar)
import MetaVar as MetaVar
import Pantograph.RevList as RevList
import Pantograph.Trifunctor (class Trifunctor)
import Pantograph.Utility (todo)
import SuperType (class SuperTypeChain, class SuperTypeStep, inject)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- SortL
--------------------------------------------------------------------------------

class PrettyTreeL d <= IsSortL d

--------------------------------------------------------------------------------
-- MetaL
--------------------------------------------------------------------------------

type MetaL l = (metaVar :: MetaVar | l)

_metaVar = Proxy :: Proxy "metaVar"

-- makeMetaVarExpr :: forall l. String -> Tree (MetaL l)
-- makeMetaVarExpr x = MetaVar (MetaVar.MetaVar x) %* []

-- makeMetaVarAndExpr :: forall l. String -> Proxy l -> MetaVar /\ Tree (MetaL l)
-- makeMetaVarAndExpr x _ = MetaVar.MetaVar x /\ makeMetaVarExpr x

-- makeMetaVarExpr' :: forall l. MetaVar -> Tree (MetaL l)
-- makeMetaVarExpr' x = MetaVar x %* []

--------------------------------------------------------------------------------
-- DerL
--------------------------------------------------------------------------------

data DerL d s = DerL (Variant d) (MetaVar.Subst (TreeV s))

makeDerL d sigma = DerL d (sigma # Map.fromFoldable)

infix 4 makeDerL as //

--------------------------------------------------------------------------------
-- DerRule
--------------------------------------------------------------------------------

type DerRules d s = Map (Variant d) (DerRule s)

class HasDerRules d s where
  derRules :: DerRules d s

data DerRule s = DerRule
  { sort :: TreeV (MetaL s)
  , kids :: List { sort :: TreeV (MetaL s) }
  }

_sort = Proxy :: Proxy "sort"
_kids = Proxy :: Proxy "kids"

makeDerRule sort kids = DerRule
  { sort
  , kids: kids # map (\s -> { sort: s }) # List.fromFoldable
  }

infix 1 makeDerRule as -|

makeDerRuleFlipped = flip makeDerRule

infix 1 makeDerRuleFlipped as |-

--------------------------------------------------------------------------------
-- AdjL
--------------------------------------------------------------------------------

type AdjL d ch s l = (bdry :: BdryDir /\ TreeV ch, der :: DerL d s | l)

data BdryDir = Up | Down

-- makeAdjBdryDownPlus l ls kid rs = InjMetaL (Plus (Tooth l (RevList.fromFoldable ls) (List.fromFoldable rs))) %* [ kid ]
-- makeAdjBdryDownMinus l ls kid rs = InjMetaL (Minus (Tooth l (RevList.fromFoldable ls) (List.fromFoldable rs))) %* [ kid ]

-- infixl 2 makeAdjBdryDownPlus as %+
-- infixl 2 makeAdjBdryDownMinus as %-

-- applyFunction f a = f a

-- infixl 2 applyFunction as <<
-- infixl 2 applyFunction as >>

-- makeAdjBdryDown :: forall d ch s ls l. SuperTypeChain l ls (AdjL d ch s) => Tree ch -> Tree l -> Tree l
-- makeAdjBdryDown ch kid = inject (AdjBdry Down ch :: AdjL d ch s) %* [ kid ]

-- infix 2 makeAdjBdryDown as ↓

-- makeAdjBdryUp :: forall d ch s ls l. SuperTypeChain l ls (AdjL d ch s) => Tree ch -> Tree l -> Tree l
-- makeAdjBdryUp ch kid = inject (AdjBdry Up ch :: AdjL d ch s) %* [ kid ]

-- infix 2 makeAdjBdryUp as ↑

-- type AdjExpr d s = Tree (AdjRule d s)

-- type AdjPat d s = Tree (MetaL (AdjL d (MetaL (ChangeL s)) (MetaL s)))

type AdjSubst d s =
  { adjs :: MetaVar.Subst (TreeV (AdjL d (ChangeL s) s ()))
  , chs :: MetaVar.Subst (TreeV (ChangeL s))
  , sorts :: MetaVar.Subst (TreeV s)
  }

-- _adjs = Proxy :: Proxy "adjs"
-- _chs = Proxy :: Proxy "chs"
-- _sorts = Proxy :: Proxy "sorts"

-- --------------------------------------------------------------------------------
-- -- AdjRules
-- --------------------------------------------------------------------------------

class HasAdjRules d s where
  adjRules :: AdjRules d s

type AdjRules d s = List (AdjRule d s)

data AdjRule d s = AdjRule
  { atTop :: Maybe Boolean
  , input :: TreeV (MetaL (AdjL d (MetaL (ChangeL s)) (MetaL s) ()))
  , trans :: AdjSubst d s -> Maybe (AdjSubst d s)
  , output :: TreeV (MetaL (AdjL d (MetaL (ChangeL s)) (MetaL s) ()))
  }

makeAdjRule input trans output = AdjRule { atTop: empty, input, trans: trans >>> map \{ sorts, adjs, chs } -> { sorts: Map.fromFoldable sorts, adjs: Map.fromFoldable adjs, chs: Map.fromFoldable chs }, output }
makeAdjTopRule input output = AdjRule { atTop: pure true, input, trans: pure, output }
makeSimpleAdjRule input output = AdjRule { atTop: empty, input, trans: pure, output }
makeSimpleTopAdjRule input output = AdjRule { atTop: pure true, input, trans: pure, output }

-- --------------------------------------------------------------------------------
-- -- match stuff
-- --------------------------------------------------------------------------------

-- matchTree
--   :: forall l
--    . Tree (MetaL l)
--   -> Tree l
--   -> Maybe (MetaVar.Subst (Tree l))
-- matchTree = todo "matchTree"

-- -- matchTreeDerL
-- --   :: forall d s
-- --    . Tree (MetaL (DerL d (MetaL s)))
-- --   -> Tree (DerL d s)
-- --   -> Maybe (MetaVar.Subst (Tree (DerL d s)) /\ MetaVar.Subst (Tree s))
-- -- matchTreeDerL = todo "matchTreeDerL"

-- matchTreeAdjL
--   :: forall d s
--    . Tree (MetaL (AdjL d (MetaL (ChangeL s)) (MetaL s)))
--   -> Tree (AdjL d (ChangeL s) s)
--   -> Maybe (AdjSubst d s)
-- matchTreeAdjL = todo "matchTreeAdjL"


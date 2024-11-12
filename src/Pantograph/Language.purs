module Pantograph.Language where

import Pantograph.Tree
import Prelude

import Control.Alternative (empty)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as V
import MetaVar (MetaVar)
import MetaVar as MetaVar
import Pantograph.RevList as RevList
import Pantograph.Utility (todo)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- SortL
--------------------------------------------------------------------------------

class (Eq d, Ord d, PrettyTreeL d) <= IsSortL d

--------------------------------------------------------------------------------
-- MetaL
--------------------------------------------------------------------------------

type MetaL l = (metaVar :: MetaVar | l)

_metaVar = Proxy :: Proxy "metaVar"

makeMetaVarTreeV :: forall l. String -> TreeV (metaVar :: MetaVar | l)
makeMetaVarTreeV x = V.inj _metaVar (MetaVar.MetaVar x) %* []

makeMetaVarAndTreeV :: forall l. String -> MetaVar /\ TreeV (metaVar :: MetaVar | l)
makeMetaVarAndTreeV x = MetaVar.MetaVar x /\ makeMetaVarTreeV x

--------------------------------------------------------------------------------
-- DerL
--------------------------------------------------------------------------------

type DerL d s l = (der :: Der d s | l)

_der = Proxy :: Proxy "der"

data Der d s = Der (Variant d) (MetaVar.Subst (TreeV s))

makeDer d sigma = Der d (sigma # Map.fromFoldable)

infix 4 makeDer as //

class (Eq d, Ord d, PrettyTreeL d) <= IsDerL d

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

type AdjL ch l = (bdry :: Bdry ch | l)

data Bdry ch = Bdry BdryDir (TreeV ch)

_bdry = Proxy :: Proxy "bdry"

data BdryDir = Up | Down

makeAdjBdryDownPlus l ls kid rs = V.inj _plus (Tooth l (RevList.fromFoldable ls) (List.fromFoldable rs)) %* [ kid ]
makeAdjBdryDownMinus l ls kid rs = V.inj _minus (Tooth l (RevList.fromFoldable ls) (List.fromFoldable rs)) %* [ kid ]

infixl 2 makeAdjBdryDownPlus as %+
infixl 2 makeAdjBdryDownMinus as %-

applyFunction f a = f a

infixl 2 applyFunction as <<
infixl 2 applyFunction as >>

makeAdjBdryDown :: forall l ch. TreeV ch -> TreeV (bdry :: Bdry ch | l) -> TreeV (bdry :: Bdry ch | l)
makeAdjBdryDown ch kid = V.inj _bdry (Bdry Down ch) %* [ kid ]

infix 2 makeAdjBdryDown as ↓

makeAdjBdryUp :: forall l ch. TreeV ch -> TreeV (bdry :: Bdry ch | l) -> TreeV (bdry :: Bdry ch | l)
makeAdjBdryUp ch kid = V.inj _bdry (Bdry Up ch) %* [ kid ]

infix 2 makeAdjBdryUp as ↑

type AdjSubst d s =
  { adjs :: MetaVar.Subst (TreeV (AdjL (ChangeL s) (DerL d s ())))
  , chs :: MetaVar.Subst (TreeV (ChangeL s))
  , sorts :: MetaVar.Subst (TreeV s)
  }

_adjs = Proxy :: Proxy "adjs"
_chs = Proxy :: Proxy "chs"
_sorts = Proxy :: Proxy "sorts"

--------------------------------------------------------------------------------
-- AdjRules
--------------------------------------------------------------------------------

class HasAdjRules d s where
  adjRules :: AdjRules d s

type AdjRules d s = List (AdjRule d s)

data AdjRule d s = AdjRule
  { atTop :: Maybe Boolean
  , input :: TreeV (MetaL (AdjL (MetaL (ChangeL s)) (DerL d (MetaL s) ())))
  , trans :: AdjSubst d s -> Maybe (AdjSubst d s)
  , output :: TreeV (MetaL (AdjL (MetaL (ChangeL s)) (DerL d (MetaL s) ())))
  }

makeAdjRule input trans output = AdjRule { atTop: empty, input, trans: trans >>> map \{ sorts, adjs, chs } -> { sorts: Map.fromFoldable sorts, adjs: Map.fromFoldable adjs, chs: Map.fromFoldable chs }, output }
makeAdjTopRule input output = AdjRule { atTop: pure true, input, trans: pure, output }
makeSimpleAdjRule input output = AdjRule { atTop: empty, input, trans: pure, output }
makeSimpleTopAdjRule input output = AdjRule { atTop: pure true, input, trans: pure, output }

--------------------------------------------------------------------------------
-- match stuff
--------------------------------------------------------------------------------

matchTreeAdjL
  :: forall d s
   . TreeV (MetaL (AdjL (MetaL (ChangeL s)) (DerL d (MetaL s) ())))
  -> TreeV (AdjL (ChangeL s) (DerL d s ()))
  -> Maybe (AdjSubst d s)
matchTreeAdjL = todo "matchTreeAdjL"


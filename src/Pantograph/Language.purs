module Pantograph.Language where

import Pantograph.Tree
import Prelude

import Control.Alternative (empty)
import Data.Foldable (fold)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant as V
import Pantograph.MetaVar (MetaVar)
import Pantograph.MetaVar as MetaVar
import Pantograph.RevList as RevList
import Pantograph.Utility (expand1, todo, uniqueList)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- SortL
--------------------------------------------------------------------------------

type SortL :: Type -> Row Type -> Row Type
type SortL s l = (sort :: s | l)

_sort = Proxy :: Proxy "sort"

makeSort s kids = V.inj _sort s % kids

infix 3 makeSort as %^

class (Eq s, Ord s, PrettyTreeL s) <= IsSortL s

--------------------------------------------------------------------------------
-- MetaL
--------------------------------------------------------------------------------

type MetaL l = (metaVar :: MetaVar | l)

_metaVar = Proxy :: Proxy "metaVar"

makeMetaVar :: forall l. MetaVar -> TreeV (metaVar :: MetaVar | l)
makeMetaVar x = V.inj _metaVar x % []

defAndMakeMetaVar :: forall l. String -> MetaVar /\ TreeV (metaVar :: MetaVar | l)
defAndMakeMetaVar str = x /\ makeMetaVar x
  where
  x = MetaVar.MetaVar str

makeMetaVar' :: forall l. String -> TreeV (metaVar :: MetaVar | l)
makeMetaVar' x = V.inj _metaVar (MetaVar.MetaVar x) % []

renameMVs :: forall l. (MetaVar -> MetaVar) -> TreeV (MetaL l) -> TreeV (MetaL l)
renameMVs f = map
  ( V.case_
      # (\_ l -> expand1 (Proxy :: Proxy "metaVar") l)
      # V.on _metaVar (\x -> V.inj _metaVar (f x))
  )

collectMVs :: forall l. TreeV (MetaL l) -> List MetaVar
collectMVs =
  map
    ( V.case_
        # mempty
        # V.on _metaVar pure
    )
    >>> fold
    >>> uniqueList

--------------------------------------------------------------------------------
-- DerL
--------------------------------------------------------------------------------

type DerL d sl l = (der :: Der d sl | l)

_der = Proxy :: Proxy "der"

data Der d sl = Der d (MetaVar.Subst (TreeV sl))

makeDer d sigma = V.inj _der $ Der d (sigma # Map.fromFoldable)

infix 5 makeDer as //

class (Eq d, Ord d, PrettyTreeL d) <= IsDerL d

--------------------------------------------------------------------------------
-- DerRule
--------------------------------------------------------------------------------

type DerRules d s = Map d (DerRule s)

class HasDerRules d s where
  derRules :: DerRules d s

data DerRule s = DerRule
  { sort :: TreeV (MetaL (SortL s ()))
  , kids :: List { sort :: TreeV (MetaL (SortL s ())) }
  }

_kids = Proxy :: Proxy "kids"

makeDerRule sort kids = DerRule
  { sort
  , kids: kids # map (\s -> { sort: s }) # List.fromFoldable
  }

infix 1 makeDerRule as -|

makeDerRuleFlipped = flip makeDerRule

infix 1 makeDerRuleFlipped as |-

class (IsDerL d, IsSortL s, HasDerRules d s) <= IsLanguage d s

--------------------------------------------------------------------------------
-- AdjL
--------------------------------------------------------------------------------

type AdjL ch l = (bdry :: Bdry ch | l)

data Bdry ch = Bdry BdryDir (TreeV ch)

_bdry = Proxy :: Proxy "bdry"

data BdryDir = Up | Down

makeAdjBdryDownPlus l ls kid rs = V.inj _plus (Tooth (V.inj _sort l) (RevList.fromFoldable ls) (List.fromFoldable rs)) % [ kid ]
makeAdjBdryDownMinus l ls kid rs = V.inj _minus (Tooth (V.inj _sort l) (RevList.fromFoldable ls) (List.fromFoldable rs)) % [ kid ]

infixl 2 makeAdjBdryDownPlus as %+
infixl 2 makeAdjBdryDownMinus as %-

applyFunction f a = f a

infixl 2 applyFunction as <<
infixl 2 applyFunction as >>

makeAdjBdryDown :: forall l ch. TreeV ch -> TreeV (AdjL ch l) -> TreeV (AdjL ch l)
makeAdjBdryDown ch kid = V.inj _bdry (Bdry Down ch) % [ kid ]

infix 2 makeAdjBdryDown as ↓

makeAdjBdryUp :: forall l ch. TreeV ch -> TreeV (AdjL ch l) -> TreeV (AdjL ch l)
makeAdjBdryUp ch kid = V.inj _bdry (Bdry Up ch) % [ kid ]

infix 2 makeAdjBdryUp as ↑

type AdjSubst d s =
  { adjs :: MetaVar.Subst (TreeV (AdjL (ChangeL (SortL s ())) (DerL d (SortL s ()) ())))
  , chs :: MetaVar.Subst (TreeV (ChangeL (SortL s ())))
  , sorts :: MetaVar.Subst (TreeV (SortL s ()))
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
  , input :: TreeV (MetaL (AdjL (MetaL (ChangeL (SortL s ()))) (DerL d (MetaL (SortL s ())) ())))
  , trans :: AdjSubst d s -> Maybe (AdjSubst d s)
  , output :: TreeV (MetaL (AdjL (MetaL (ChangeL (SortL s ()))) (DerL d (MetaL (SortL s ())) ())))
  }

makeAdjRule input output trans = AdjRule { atTop: empty, input, trans: trans >>> map \{ sorts, adjs, chs } -> { sorts: Map.fromFoldable sorts, adjs: Map.fromFoldable adjs, chs: Map.fromFoldable chs }, output }
makeAdjTopRule input output = AdjRule { atTop: pure true, input, trans: pure, output }
makeSimpleAdjRule input output = AdjRule { atTop: empty, input, trans: pure, output }
makeSimpleTopAdjRule input output = AdjRule { atTop: pure true, input, trans: pure, output }

--------------------------------------------------------------------------------
-- match stuff
--------------------------------------------------------------------------------

matchTreeAdjL
  :: forall d s
   . TreeV (MetaL (AdjL (MetaL (ChangeL (SortL s ()))) (DerL d (MetaL (SortL s ())) ())))
  -> TreeV (AdjL (ChangeL (SortL s ())) (DerL d (SortL s ()) ()))
  -> Maybe (AdjSubst d s)
matchTreeAdjL = todo "matchTreeAdjL"


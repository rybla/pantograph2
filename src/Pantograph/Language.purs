module Pantograph.Language where

import Pantograph.Tree
import Prelude

import Data.Bifunctor (class Bifunctor, bimap)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import MetaVar (MetaVar)
import MetaVar as MetaVar
import Pantograph.Trifunctor (class Trifunctor)
import Pantograph.Utility (todo)
import SuperType (class SuperType, inject)

--------------------------------------------------------------------------------
-- MetaL
--------------------------------------------------------------------------------

data MetaL l
  = MetaVar MetaVar
  | InjMetaL l

instance SuperType l l' => SuperType (MetaL l) l' where
  inject = InjMetaL <<< inject

--------------------------------------------------------------------------------
-- SortL
--------------------------------------------------------------------------------

data SortL s = InjSortL s

instance SuperType s s' => SuperType (SortL s) s' where
  inject = InjSortL <<< inject

--------------------------------------------------------------------------------
-- DerRule
--------------------------------------------------------------------------------

type DerRules d s = Map.Map d (DerRule s)

class HasDerRules d s where
  derRules :: DerRules d s

data DerRule s = DerRule
  { sort :: Tree (MetaL s)
  , kids :: List { sort :: Tree (MetaL s) }
  }

--------------------------------------------------------------------------------
-- DerL
--------------------------------------------------------------------------------

data DerL d s = DerL d (MetaVar.Subst (Tree s))

derive instance Generic (DerL d s) _
derive instance Functor (DerL d)
derive instance Bifunctor DerL

instance (SuperType d d', SuperType s s') => SuperType (DerL d s) (DerL d' s') where
  inject = bimap inject inject

--------------------------------------------------------------------------------
-- AdjRules
--------------------------------------------------------------------------------

class HasAdjRules d s where
  adjRules :: AdjRules d s

data AdjRules d s = AdjRules
  { upTopRule :: UpTopRule d s
  , upRules :: List (UpAdjRule d s)
  , downRules :: List (DownAdjRule d s)
  }

type UpTopRule d s =
  Tree (ChangeL (SortL s)) /\ Tree (AdjL d (ChangeL (SortL s)) (SortL s))
  -> Maybe (Tree (AdjL d (ChangeL (SortL s)) (SortL s)))

type UpAdjRule d s =
  Tooth (AdjL d (ChangeL (SortL s)) (SortL s)) /\ Tree (ChangeL (SortL s))
  -> Maybe
       ( Maybe (Tree (ChangeL (SortL s)))
           /\ Path (AdjL d (ChangeL (SortL s)) (SortL s))
           /\ Maybe (Tree (ChangeL (SortL s)))
       )

type DownAdjRule d s =
  Tree (ChangeL (SortL s)) /\ Tree (AdjL d (ChangeL (SortL s)) (SortL s))
  -> Maybe (AdjL d (ChangeL (SortL s)) (SortL s))

--------------------------------------------------------------------------------
-- AdjL
--------------------------------------------------------------------------------

data AdjL d ch s
  = AdjBdry BdryDir (Tree ch)
  | InjAdjL (DerL d s)

derive instance Generic (AdjL d ch s) _

derive instance Functor (AdjL d ch)
derive instance Bifunctor (AdjL d)

instance Trifunctor AdjL where
  trimap = todo "trimap@Adj"

data BdryDir = Up | Down

instance (SuperType d d', SuperType ch ch', SuperType s s') => SuperType (AdjL d ch s) (AdjL d' ch' s') where
  inject (AdjBdry dir ch) = AdjBdry dir (ch # map inject)
  inject (InjAdjL l) = InjAdjL (inject l)

--------------------------------------------------------------------------------
-- match stuff
--------------------------------------------------------------------------------

matchTree
  :: forall l
   . Tree (MetaL l)
  -> Tree l
  -> Maybe (MetaVar.Subst (Tree l))
matchTree = todo "matchTree"

matchTreeDerL
  :: forall d s
   . Tree (MetaL (DerL d (MetaL s)))
  -> Tree (DerL d s)
  -> Maybe (MetaVar.Subst (Tree (DerL d s)) /\ MetaVar.Subst (Tree s))
matchTreeDerL = todo "matchTreeDerL"

matchTreeAdjL
  :: forall d s
   . Tree (MetaL (AdjL d (MetaL (ChangeL s)) (MetaL s)))
  -> Tree (AdjL d (ChangeL s) s)
  -> Maybe (MetaVar.Subst (Tree (AdjL d (ChangeL s) s)) /\ MetaVar.Subst (Tree s))
matchTreeAdjL = todo "matchTreeAdjL"


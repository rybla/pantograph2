module Pantograph.Unification where

import Pantograph.Grammar
import Prelude

import Control.Alternative (guard)
import Control.Monad.State (State, StateT, execStateT, modify_)
import Data.Either (Either(..))
import Data.Foldable (length, traverse_)
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Pantograph.Tree ((▵))
import Pantograph.Utility (bug, todo)

unifyMetaSortChanges :: forall s. MetaSortChange s -> MetaSortChange s -> Maybe (MetaVarSubst (MetaSortChange s))
unifyMetaSortChanges _ _ = todo "unifyMetaSortChanges"

unifyMetaSorts :: forall s. Eq s => MetaSort s -> MetaSort s -> Maybe (MetaVarSubst (MetaSort s))
unifyMetaSorts m1_init m2_init = do
  sigma <- execStateT (go m1_init m2_init) Map.empty
  -- TODO: saturate substitution and check for loops
  pure sigma
  where
  go :: MetaSort s -> MetaSort s -> StateT (MetaVarSubst _) Maybe Unit
  go (Left x1 ▵ ms1) (Left x2 ▵ ms2) = todo ""

  go (Left x1 ▵ Nil) m2@(Right _ ▵ _) = modify_ (Map.insert x1 m2)
  go (Left _ ▵ _) (Right _ ▵ _) = bug ""

  go m1@(Right _ ▵ _) (Left x2 ▵ Nil) = modify_ (Map.insert x2 m1)
  go (Right _ ▵ _) (Left _ ▵ _) = bug ""

  go (Right s1 ▵ ms1) (Right s2 ▵ ms2) = do
    guard $ s1 == s2
    if not $ length ms1 == (length ms2 :: Int) then bug $ "Sorts of the same label must have the same number of kids" else pure unit
    List.zip ms1 ms2 # traverse_ \(m1 /\ m2) -> go m1 m2
    pure unit

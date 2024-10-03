module Pantograph.Propagation where

import Pantograph.Grammar
import Prelude

import Control.Plus (empty)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Pantograph.Tree (getTeeth, stepPath, unstepPath)

-- TODO: is there a better way of writing this short-circuited rather than
-- expanded recursion over Lists?
propagateOnce :: forall d s. PropagRules d s -> PropagDerivPath d s -> PropagDeriv d s -> Maybe (PropagDeriv d s)
propagateOnce prs path pd = case propagateOnce' prs (unstepPath path <#> snd) pd of
  Just pd' -> pure pd'
  Nothing -> pd # getTeeth # go
    where
    go Nil = empty
    go ((th /\ t) : steps) = case propagateOnce prs (path `stepPath` th) t of
      Just pd' -> pure pd'
      Nothing -> go steps

propagateOnce' :: forall d s. PropagRules d s -> Maybe (PropagDerivTooth d s) -> PropagDeriv d s -> Maybe (PropagDeriv d s)
propagateOnce' Nil _ _ = empty
propagateOnce' (PropagRule _name pr : prs) mb_th pd = case pr mb_th pd of
  Nothing -> propagateOnce' prs mb_th pd
  Just pd' -> pure pd'

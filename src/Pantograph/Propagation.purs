module Pantograph.Propagation where

import Pantograph.Language
import Pantograph.Tree
import Prelude

import Control.Plus (empty)
import Data.Foldable (intercalate)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Debug as Debug
import Pantograph.EitherF (EitherF(..))
import Pantograph.Pretty (pretty)
import Pantograph.Utility (bug)

fromPropagDerivToDeriv :: forall d s. Tree (PropagDerivLabel d s) -> Tree (DerivLabel d s)
fromPropagDerivToDeriv (LeftF (PropagBoundary _ ch) % (kid : Nil)) = DerivBoundary ch % ((kid # fromPropagDerivToDeriv) : Nil)
fromPropagDerivToDeriv (RightF dl % kids) = dl % (kids # map fromPropagDerivToDeriv)
fromPropagDerivToDeriv _ = bug "invalid"

propagateFixpoint :: forall d s. PrettyTreeLabel d => PrettyTreeLabel s => PropagRules d s -> Tree (PropagDerivLabel d s) -> Tree (PropagDerivLabel d s)
propagateFixpoint prs pd0 = go pd0
  where
  go pd = case propagateOnce prs mempty pd of
    Nothing -> pd
    Just (PropagRule { name } /\ pd') -> Debug.trace
      ( intercalate "\n"
          [ "[propagateFixpoint] successfully propagated a step:"
          , "[propagateFixpoint]   - name : " <> name
          , "[propagateFixpoint]   - pd   : " <> pretty pd
          , "[propagateFixpoint]   - pd'  : " <> pretty pd'
          ]
      )
      \_ ->
        go pd'

-- TODO: is there a better way of writing this short-circuited rather than
-- expanded recursion over Lists?
propagateOnce :: forall d s. PropagRules d s -> Path (PropagDerivLabel d s) -> Tree (PropagDerivLabel d s) -> Maybe (PropagRule d s /\ Tree (PropagDerivLabel d s))
propagateOnce prs path pd = case propagateOnce' prs (unstepPath path <#> snd) pd of
  Just (pr /\ pd') -> pure $ pr /\ unPath path pd'
  Nothing -> pd # getTeeth # go
    where
    go Nil = empty
    go ((th /\ t) : steps) = case propagateOnce prs (path `stepPath` th) t of
      Just pd' -> pure pd'
      Nothing -> go steps

propagateOnce' :: forall d s. PropagRules d s -> Maybe (Tooth (PropagDerivLabel d s)) -> Tree (PropagDerivLabel d s) -> Maybe (PropagRule d s /\ Tree (PropagDerivLabel d s))
propagateOnce' Nil _ _ = empty
propagateOnce' (pr0@(PropagRule { rule }) : prs) mb_th pd = case rule mb_th pd of
  Nothing -> propagateOnce' prs mb_th pd
  Just pd' -> pure $ pr0 /\ pd'

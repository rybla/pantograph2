module Pantograph.Adjust where

import Pantograph.Language
import Pantograph.Tree
import Prelude

import Control.Plus (empty)
import Data.Foldable (intercalate)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (traverse)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Debug as Debug
import Pantograph.EitherF (EitherF(..))
import Pantograph.Pretty (pretty)
import Pantograph.Utility (bug, todo)

fixpoint :: forall a. (a -> Maybe a) -> a -> a
fixpoint f a = case f a of
  Nothing -> a
  Just a' -> fixpoint f a'

traverseFirst :: forall a b. (a -> Maybe b) -> List a -> Maybe b
traverseFirst _ Nil = empty
traverseFirst f (x : xs) = case f x of
  Nothing -> traverseFirst f xs
  Just y -> pure y

propagateStep :: forall d s. AdjRules d s -> Tree (AdjLbl d s) -> Maybe (Tree (AdjLbl d s))
propagateStep (AdjRules { upRules, downRules }) t = go mempty t
  where
  go :: Path (AdjLbl d s) -> Tree (AdjLbl d s) -> Maybe (Tree (AdjLbl d s))
  go path t@(AdjBdry dir ch % (kid : Nil)) = todo ""
  go path t@((AdjBdry dir ch) % _) = bug "invalid AdjBdry"
  go path t@((InjAdjLbl dl) % kids) = todo ""

-- case getTeeth t # traverseFirst f of
--   Nothing -> ?a
--   Just (th /\ t') -> pure ?a
-- where
-- f = ?a

-- fromAdjustDerivToDeriv :: forall d s. Tree (AdjustLbl d s) -> Tree (DerLbl d s)
-- fromAdjustDerivToDeriv (AdjustBdry _ ch % (kid : Nil)) = DerBdry ch % ((kid # fromAdjustDerivToDeriv) : Nil)
-- fromAdjustDerivToDeriv (InjAdjustLbl dl % kids) = dl % (kids # map fromAdjustDerivToDeriv)
-- fromAdjustDerivToDeriv _ = bug "invalid"

-- propagateFixpoint :: forall d s. PrettyTreeLbl d => PrettyTreeLbl s => AdjustRules d s -> Tree (AdjustLbl d s) -> Tree (AdjustLbl d s)
-- propagateFixpoint prs pd0 = go pd0
--   where
--   go pd = case propagateOnce prs mempty pd of
--     Nothing -> pd
--     Just (AdjustRule { name } /\ pd') -> Debug.trace
--       ( intercalate "\n"
--           [ "[propagateFixpoint] successfully propagated a step:"
--           , "[propagateFixpoint]   - name : " <> name
--           , "[propagateFixpoint]   - pd   : " <> pretty pd
--           , "[propagateFixpoint]   - pd'  : " <> pretty pd'
--           ]
--       )
--       \_ ->
--         go pd'

-- -- TODO: is there a better way of writing this short-circuited rather than
-- -- expanded recursion over Lists?
-- propagateOnce :: forall d s. AdjustRules d s -> Path (AdjustLbl d s) -> Tree (AdjustLbl d s) -> Maybe (AdjustRule d s /\ Tree (AdjustLbl d s))
-- propagateOnce prs path pd = case propagateOnce' prs (unstepPath path <#> snd) pd of
--   Just (pr /\ pd') -> pure $ pr /\ unPath path pd'
--   Nothing -> pd # getTeeth # go
--     where
--     go Nil = empty
--     go ((th /\ t) : steps) = case propagateOnce prs (path `stepPath` th) t of
--       Just pd' -> pure pd'
--       Nothing -> go steps

-- propagateOnce' :: forall d s. AdjustRules d s -> Maybe (Tooth (AdjustLbl d s)) -> Tree (AdjustLbl d s) -> Maybe (AdjustRule d s /\ Tree (AdjustLbl d s))
-- propagateOnce' Nil _ _ = empty
-- propagateOnce' (pr0@(AdjustRule { rule }) : prs) mb_th pd = case rule mb_th pd of
--   Nothing -> propagateOnce' prs mb_th pd
--   Just pd' -> pure $ pr0 /\ pd'

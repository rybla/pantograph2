module Pantograph.Adjust where

import Pantograph.Language
import Pantograph.Tree
import Prelude

import Control.Plus (empty)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Pantograph.Utility (bug)

fixpoint :: forall a. (a -> Maybe a) -> a -> a
fixpoint f a = case f a of
  Nothing -> a
  Just a' -> fixpoint f a'

tryFirst :: forall a b. (a -> Maybe b) -> List a -> Maybe b
tryFirst _ Nil = empty
tryFirst f (x : xs) = case f x of
  Nothing -> tryFirst f xs
  Just y -> pure y

propagate :: forall d s. HasAdjRules d s => Tree (AdjLbl d s) -> Tree (AdjLbl d s)
propagate = fixpoint propagateStep

propagateStep :: forall d s. HasAdjRules d s => Tree (AdjLbl d s) -> Maybe (Tree (AdjLbl d s))
propagateStep t0 = go mempty t0
  where
  AdjRules { upRules, downRules, upTopRule } = adjRules :: AdjRules d s

  go :: Path (AdjLbl d s) -> Tree (AdjLbl d s) -> Maybe (Tree (AdjLbl d s))
  go path (AdjBdry Up ch % (t' : Nil)) = case unstepPath path of
    Nothing -> upTopRule ch t'
    Just (path' /\ th) -> do
      { up, mid, down } <- upRules # tryFirst (\f -> f th ch)
      pure
        $ unPath path'
        $ (up # maybe identity \upCh kid -> AdjBdry Up upCh %* [ kid ])
        $ unPath mid
        $ (down # maybe identity \downCh kid -> AdjBdry Down downCh %* [ kid ])
        $ t'
  go path (AdjBdry Down ch % (t' : Nil)) = do
    { up, mid, down } <- downRules # tryFirst (\f -> f ch t')
    pure
      $ unPath path
      $ (up # maybe identity \upCh kid -> AdjBdry Up upCh %* [ kid ])
      $ unPath mid
      $ (down # maybe identity \downCh kid -> AdjBdry Down downCh %* [ kid ])
      $ t'
  go _ (AdjBdry _ _ % _) = bug "invalid AdjBdry"
  go path t = getTeeth t # tryFirst \(th /\ t') -> go (stepPath path th) t'

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

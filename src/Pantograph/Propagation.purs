module Pantograph.Propagation where

import Pantograph.Language
import Pantograph.Tree
import Prelude

import Control.Plus (empty)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Pantograph.Utility (bug, fixpoint, tryFirst)

propagate :: forall d s. HasAdjRules d s => Tree (AdjLbl d s) -> Tree (AdjLbl d s)
propagate = fixpoint propagateStep

propagateStep :: forall d s. HasAdjRules d s => Tree (AdjLbl d s) -> Maybe (Tree (AdjLbl d s))
propagateStep t0 = go mempty t0
  where
  AdjRules { upRules, downRules, upTopRule } = adjRules :: AdjRules d s

  go :: Path (AdjLbl d s) -> Tree (AdjLbl d s) -> Maybe (Tree (AdjLbl d s))
  go path (AdjBdry Up ch % (t' : Nil)) = case unstepPath path of
    Nothing -> upTopRule (ch /\ t')
    Just (path' /\ th) -> do
      { up, mid, down } <- upRules # tryFirst (_ $ th /\ ch)
      pure
        $ unPath path'
        $ (up # maybe identity \upCh kid -> AdjBdry Up upCh %* [ kid ])
        $ unPath mid
        $ (down # maybe identity \downCh kid -> AdjBdry Down downCh %* [ kid ])
        $ t'
  go path (AdjBdry Down ch % (t' : Nil)) = do
    t'' <- downRules # tryFirst (_ $ ch /\ t')
    -- pure
    --   $ unPath path
    --   $ (up # maybe identity \upCh kid -> AdjBdry Up upCh %* [ kid ])
    --   $ (down # maybe identity \downCh kid -> AdjBdry Down downCh %* [ kid ])
    --   $ t'
    pure
      $ unPath path
      $ t''
  go _ (AdjBdry _ _ % _) = bug "invalid AdjBdry"
  go path t = getTeeth t # tryFirst \(th /\ t') -> go (stepPath path th) t'

fromAdjToDer :: forall d s. Tree (AdjLbl d s) -> Maybe (Tree (DerLbl d s))
fromAdjToDer = traverse case _ of
  AdjBdry _ _ -> empty
  InjAdjLbl dl -> pure dl

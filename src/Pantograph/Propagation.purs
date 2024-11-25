module Pantograph.Propagation where

import Pantograph.Language
import Pantograph.Tree
import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.Writer (class MonadWriter, tell)
import Data.Either.Nested (type (\/))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant as V
import Pantograph.Utility (todo, tryFirst)

type PropagationLog d l_d s l_s = AdjT d l_d s l_s

-- | Does fixpoint of `propagateStep`, then attempts to project away `AdjL`
-- | label (can throw the tree that still has `AdjL` label).
-- |
-- | `MonadWriter` tells each application of an `AdjRule` and the resulting
-- | tree.
propagate
  :: forall m d l_d s l_s
   . MonadWriter (List (PropagationLog d l_d s l_s)) m
  => IsAdjLanguage d s
  => AdjT d l_d s l_s
  -> m (AdjT d l_d s l_s \/ DerT d l_d s l_s)
propagate t = propagateStep t # runMaybeT >>= case _ of
  Nothing -> pure $ t # traverse
    ( V.case_
        # const pure
        # V.on _bdry (const (throwError t))
    )
  Just t' -> propagate t'

-- | Attempts to apply an `AdjRule` at a single point in a tree (or `empty` if
-- | no possible applications).
propagateStep
  :: forall d l_d s l_s m
   . MonadWriter (List (PropagationLog d l_d s l_s)) m
  => IsAdjLanguage d s
  => AdjT d l_d s l_s
  -> MaybeT m (AdjT d l_d s l_s)
propagateStep t0 = go mempty t0
  where
  rules = adjRules :: AdjRules d s

  go
    :: PathV (AdjL d l_d s l_s)
    -> AdjT d l_d s l_s
    -> MaybeT m (AdjT d l_d s l_s)
  -- go path t = case (\rule -> (rule /\ _) <$> rule `applyAdjRule` t) `tryFirst` rules of
  --   Just (_rule /\ t') -> do
  --     let t'' = unPath path t'
  --     tell $ pure t''
  --     pure t''
  --   Nothing -> (\(th /\ t') -> go (path `stepPath` th) t') `tryFirst` getTeeth t
  go path t = todo "generalize applyAdjRule over l_d, l_s"

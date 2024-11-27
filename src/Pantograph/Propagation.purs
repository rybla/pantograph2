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
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Data.Variant as V
import Pantograph.Utility (tryFirst, (##))

type PropagationLog dr sr = AdjDer dr sr

-- | Does fixpoint of `propagateStep`, then attempts to project away `AdjDerL`
-- | label (can throw the tree that still has `AdjDerL` label).
-- |
-- | `MonadWriter` tells each application of an `AdjRule` and the resulting
-- | tree.
propagate
  :: forall m dr sr
   . MonadWriter (List (PropagationLog dr sr)) m
  => Eq (Variant dr)
  => Eq (SortL sr)
  => Eq (ChangeSortL sr)
  => Eq (AdjDerL dr sr)
  => AdjDerRules dr sr
  -> AdjDer dr sr
  -> m (AdjDer dr sr \/ Der dr sr)
propagate adjRules t = propagateStep adjRules t # runMaybeT >>= case _ of
  Nothing -> pure $ t # traverse
    ( \(DerL dl sigma) ->
        dl ## V.case_
          # (\_ dl' -> pure $ DerL dl' sigma)
          # V.on _bdry (const (throwError t))
    )
  Just t' -> propagate adjRules t'

-- | Attempts to apply an `AdjRule` at a single point in a tree (or `empty` if
-- | no possible applications).
propagateStep
  :: forall dr sr m
   . MonadWriter (List (PropagationLog dr sr)) m
  => Eq (Variant dr)
  => Eq (SortL sr)
  => Eq (ChangeSortL sr)
  => Eq (AdjDerL dr sr)
  => AdjDerRules dr sr
  -> AdjDer dr sr
  -> MaybeT m (AdjDer dr sr)
propagateStep adjRules t0 = go mempty t0
  where
  go
    :: Path (AdjDerL dr sr)
    -> AdjDer dr sr
    -> MaybeT m (AdjDer dr sr)
  go path t = case (\rule -> (rule /\ _) <$> rule `applyAdjDerRule` t) `tryFirst` adjRules of
    Just (_rule /\ t') -> do
      let t'' = unPath path t'
      tell $ pure t''
      pure t''
    Nothing -> (\(th /\ t') -> go (path `stepPath` th) t') `tryFirst` getTeeth t

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
import Pantograph.Utility (tryFirst)

-- | Does fixpoint of `propagateStep`, then attempts to project away `AdjL`
-- | label (can throw the tree that still has `AdjL` label).
-- |
-- | `MonadWriter` tells each application of an `AdjRule` and the resulting
-- | tree.
propagate
  :: forall m d s
   . MonadWriter (List (AdjRule d s /\ TreeV (AdjL (ChangeL (SortL s ())) (DerL d (SortL s ()) ())))) m
  => HasAdjRules d s
  => TreeV (AdjL (ChangeL (SortL s ())) (DerL d (SortL s ()) ()))
  -> m
       ( TreeV (AdjL (ChangeL (SortL s ())) (DerL d (SortL s ()) ())) \/
           TreeV (DerL d (SortL s ()) ())
       )
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
  :: forall d s m
   . MonadWriter (List (AdjRule d s /\ TreeV (AdjL (ChangeL (SortL s ())) (DerL d (SortL s ()) ())))) m
  => HasAdjRules d s
  => TreeV (AdjL (ChangeL (SortL s ())) (DerL d (SortL s ()) ()))
  -> MaybeT m (TreeV (AdjL (ChangeL (SortL s ())) (DerL d (SortL s ()) ())))
propagateStep t0 = go mempty t0
  where
  rules = adjRules :: AdjRules d s

  go
    :: PathV (AdjL (ChangeL (SortL s ())) (DerL d (SortL s ()) ()))
    -> TreeV (AdjL (ChangeL (SortL s ())) (DerL d (SortL s ()) ()))
    -> MaybeT m (TreeV (AdjL (ChangeL (SortL s ())) (DerL d (SortL s ()) ())))
  go path t = case (\rule -> (rule /\ _) <$> rule `applyAdjRule` t) `tryFirst` rules of
    Just (rule /\ t') -> do
      let t'' = unPath path t'
      tell $ pure $ rule /\ t''
      pure t''
    Nothing -> (\(th /\ t') -> go (path `stepPath` th) t') `tryFirst` getTeeth t

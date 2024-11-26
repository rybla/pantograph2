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
import Pantograph.Utility (tryFirst)

type PropagationLog d l_d s l_s = AdjT d l_d s l_s

-- | Does fixpoint of `propagateStep`, then attempts to project away `AdjL`
-- | label (can throw the tree that still has `AdjL` label).
-- |
-- | `MonadWriter` tells each application of an `AdjRule` and the resulting
-- | tree.
propagate
  :: forall m d l_d s l_s
   . MonadWriter (List (PropagationLog d l_d s l_s)) m
  => Eq (Variant l_d)
  => Eq (Variant (SortL s l_s))
  => Eq (Variant (SortChL s l_s))
  => Eq (Variant (AdjL d l_d s l_s))
  => IsLanguage d s
  => AdjRules d l_d s l_s
  -> AdjT d l_d s l_s
  -> m (AdjT d l_d s l_s \/ DerT d l_d s l_s)
propagate adjRules t = propagateStep adjRules t # runMaybeT >>= case _ of
  Nothing -> pure $ t # traverse
    ( V.case_
        # const pure
        # V.on _bdry (const (throwError t))
    )
  Just t' -> propagate adjRules t'

-- | Attempts to apply an `AdjRule` at a single point in a tree (or `empty` if
-- | no possible applications).
propagateStep
  :: forall d l_d s l_s m
   . MonadWriter (List (PropagationLog d l_d s l_s)) m
  => Eq (Variant l_d)
  => Eq (Variant (SortL s l_s))
  => Eq (Variant (SortChL s l_s))
  => Eq (Variant (AdjL d l_d s l_s))
  => IsLanguage d s
  => AdjRules d l_d s l_s
  -> AdjT d l_d s l_s
  -> MaybeT m (AdjT d l_d s l_s)
propagateStep adjRules t0 = go mempty t0
  where
  go
    :: PathV (AdjL d l_d s l_s)
    -> AdjT d l_d s l_s
    -> MaybeT m (AdjT d l_d s l_s)
  go path t = case (\rule -> (rule /\ _) <$> rule `applyAdjRule` t) `tryFirst` adjRules of
    Just (_rule /\ t') -> do
      let t'' = unPath path t'
      tell $ pure t''
      pure t''
    Nothing -> (\(th /\ t') -> go (path `stepPath` th) t') `tryFirst` getTeeth t
-- go path t = todo "generalize applyAdjRule over l_d, l_s"

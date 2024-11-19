module Pantograph.Debug where

import Prelude

import Debug as Debug
import Pantograph.Config as Config

logM :: forall m a. Monad m => Int -> a -> m Unit
logM level | level >= Config.log_level = \a -> do
  Debug.traceM "────────────────────────────────────────────────────────────────────────────────"
  Debug.traceM a
logM _ = const (pure unit)

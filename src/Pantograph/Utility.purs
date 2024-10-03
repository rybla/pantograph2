module Pantograph.Utility where

import Prelude

import Partial.Unsafe (unsafeCrashWith)

todo :: forall a. String -> a
todo msg = unsafeCrashWith $ "TODO: " <> msg

unimplemented :: forall a. String -> a
unimplemented label = unsafeCrashWith $ "unimplemented: " <> label

bug :: forall b13. String -> b13
bug msg = unsafeCrashWith $ "bug: " <> msg

assert :: forall t21. String -> Boolean -> (Unit -> t21) -> t21
assert msg b k = if not b then bug msg else k unit

assertM :: forall m. Monad m => String -> Boolean -> m Unit
assertM msg b = if not b then pure (bug msg) else pure unit

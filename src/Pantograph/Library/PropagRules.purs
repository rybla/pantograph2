module Pantograph.Library.PropagRules where

import Pantograph.Language
import Pantograph.Tree
import Prelude

import Control.Plus (empty)
import Data.List (List(..), (:))
import Data.List as List
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Debug as Debug
import Pantograph.EitherF (EitherF(..))
import Pantograph.Pretty (class Pretty, pretty)
import Pantograph.Utility (todo)

defaultPropagRules :: forall d s. IsDerivPropagLanguage d s => PropagRules d s
defaultPropagRules =
  [ defaultPassThroughDownPropagRule
  ] # List.fromFoldable

defaultPassThroughDownPropagRule :: forall d s. IsDerivPropagLanguage d s => PropagRule d s
defaultPassThroughDownPropagRule = PropagRule
  { name: "defaultPassThroughDownPropagRule"
  , rule: \_mb_th -> case _ of
      t@(LeftF (PropagBoundary Down ch) % ((RightF dl@(DerivLabel d _) % kids) : Nil)) -> do
        Debug.traceM $ "[defaultPassThroughDownPropagRule] init: " <> pretty t
        kids' <- kids `List.zip` ((derivPropagRules :: DerivPropagRules d s) d # unwrap # _.kids)
          # traverse \(kid /\ { passthrough_down }) -> do
              ch' <- passthrough_down ch
              pure $ LeftF (PropagBoundary Down ch') % (kid : Nil)
        pure $ RightF dl % kids'
      _ -> empty
  }


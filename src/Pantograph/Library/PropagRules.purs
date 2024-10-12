module Pantograph.Library.PropagRules where

import Pantograph.Grammar
import Pantograph.Tree
import Prelude

import Control.Plus (empty)
import Data.List (List(..), (:))
import Data.List as List
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Debug as Debug
import Pantograph.EitherF (EitherF(..))
import Pantograph.Pretty (class Pretty, pretty)

defaultPropagRules
  :: forall d s
   . Pretty d
  => PrettyTreeLabel d
  => Pretty s
  => PrettyTreeLabel s
  => Eq s
  => DerivRules d s
  -> PropagRules d s
defaultPropagRules = pure >>> apply
  ( [ defaultCongruenceDownPropagRule
    ] # List.fromFoldable
  )

defaultCongruenceDownPropagRule
  :: forall d s
   . Pretty d
  => PrettyTreeLabel d
  => Pretty s
  => PrettyTreeLabel s
  => Eq s
  => DerivRules d s
  -> PropagRule d s
defaultCongruenceDownPropagRule derivRules = PropagRule "defaultCongruenceDownPropagRule" \_mb_th -> case _ of
  t@(LeftF (PropagBoundary Down ch) % ((RightF dl % kids) : Nil)) -> do
    Debug.traceM $ "[defaultCongruenceDownPropagRule] init: " <> pretty t
    let chs_kids = getKidSortChangesOfDerivLabel derivRules dl
    kids' <-
      List.zip kids chs_kids # traverse \(kid /\ ch_kid) -> do
        let ch_inv_kid = ch_kid # invertChange
        Debug.traceM $ "[defaultCongruenceDownPropagRule] trying to compose kid change at:"
        Debug.traceM $ "[defaultCongruenceDownPropagRule]   - kid     : " <> pretty kid
        Debug.traceM $ "[defaultCongruenceDownPropagRule]   - ch_kid  : " <> pretty ch_kid
        Debug.traceM $ "[defaultCongruenceDownPropagRule]   - ch      : " <> pretty ch
        ch' <- ch `composeChanges` ch_kid
        Debug.traceM $ "[defaultCongruenceDownPropagRule]   - ch'     : " <> pretty ch'
        pure $ LeftF (PropagBoundary Down ch') % (kid : Nil)
    Debug.traceM $ "[defaultCongruenceDownPropagRule] composed ALL kid changes"
    pure $ RightF dl % kids'
  _ -> empty


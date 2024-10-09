module Pantograph.Library.PropagRules where

import Pantograph.Grammar
import Pantograph.Tree
import Prelude

import Control.Plus (empty)
import Data.List (List(..), (:))
import Data.List as List
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Pantograph.EitherF (EitherF(..))

defaultPropagRules :: forall d s. Eq s => DerivRules d s -> PropagRules d s
defaultPropagRules = pure >>> apply
  ( [ defaultCongruenceDownPropagRule
    ] # List.fromFoldable
  )

defaultCongruenceDownPropagRule :: forall d s. Eq s => DerivRules d s -> PropagRule d s
defaultCongruenceDownPropagRule derivRules = PropagRule "defaultCongruenceDownPropagRule" \_mb_th -> case _ of
  LeftF (PropagBoundary Down ch) ▵ ((RightF dl ▵ kids) : Nil) -> do
    let chs_kids = getKidSortChangesOfDerivLabel derivRules dl
    kids' <-
      List.zip kids chs_kids # traverse \(kid /\ ch_kid) -> do
        ch' <- ch `composeChanges` ch_kid
        pure $ LeftF (PropagBoundary Down ch') ▵ (kid : Nil)
    pure $ RightF dl ▵ kids'
  _ -> empty


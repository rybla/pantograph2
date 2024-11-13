module Pantograph.Library.DerivePropagationAdjRulesFromDerRules where

import Data.Foldable
import Pantograph.Language
import Pantograph.Tree
import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as V
import MetaVar (MetaVar, (!!))
import MetaVar as MetaVar
import Prim.Row (class Nub, class Union)
import SuperType (inject)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- expand' :: forall lt a gt gt'. Union a lt gt => Nub gt gt' => Proxy a -> Variant lt -> Variant gt'
-- expand' _ = unsafeCoerce

propagationAdjRules :: forall d s. IsLanguage d s => AdjRules d s
propagationAdjRules =
  (derRules :: DerRules d s)
    # (Map.toUnfoldable :: _ -> List _)
    # foldMap \(d /\ DerRule { sort, kids }) -> List.fromFoldable
        let
          kidMetaVars = kids # mapWithIndex \i _ -> MetaVar.MetaVar ("kid_" <> show i)
        in
          [ makeAdjRule
              -- freshen each sort metavar in output sort as a new sort change metavar which will be matched against in the down change of the adjust boundary here
              ( ( sort # map
                    ( V.case_
                        # (\_ l -> ?a) -- expand' (Proxy :: Proxy (MetaL (ChangeL s))) (l :: Variant s))
                        # V.on _metaVar (\(MetaVar.MetaVar x) -> V.inj _metaVar (MetaVar.MetaVar ("chi_" <> x)))
                    )
                )
                  ↓
                    ?A
              )
              ( \{ chs, sorts, adjs } -> pure
                  { chs: [], sorts: [], adjs: [] }
              )
              ?a
          ]

-- propagationAdjRules :: forall d s. IsLanguage d s => AdjRules d s
-- propagationAdjRules =
--   (derRules :: Map d (DerRule s))
--     # (Map.toUnfoldable :: _ -> List _)

-- collectMetaVars =
--   map
--     ( \x -> x # foldMap case _ of
--         InjMetaL l -> Nil
--         MetaVar x -> pure x
--     )
--     >>> fold
--     >>> Set.fromFoldable
--     >>> Set.toUnfoldable
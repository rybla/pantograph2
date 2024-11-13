module Pantograph.Library.DerivePropagationAdjRulesFromDerRules where

import Data.Foldable
import Pantograph.Language
import Pantograph.Tree
import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Data.Variant as V
import MetaVar (addPrefix, addSuffix)
import MetaVar as MV

propagationAdjRules :: forall d s. IsLanguage d s => AdjRules d s
propagationAdjRules =
  (derRules :: DerRules d s)
    # (Map.toUnfoldable :: _ -> List _)
    # foldMap \(d /\ DerRule rule) -> List.fromFoldable
        let
          kidMVs = rule.kids # mapWithIndex \i _ -> MV.MetaVar ("kid_" <> show i)
          -- kidSortChMVs = kidSortMVs # map (MV.addPrefix "ch")
          kidSortMVs = rule.kids # map (\{ sort: s } -> s) # foldMap collectMVs
        in
          [ makeAdjRule
              -- freshen each sort metavar in output sort as a new sort change metavar which will be matched against in the down change of the adjust boundary here
              ( (rule.sort # map V.expand # renameMVs (addPrefix "ch"))
                  ↓
                    d
                  //
                    (kidSortMVs # map (\x -> x /\ makeMetaVar x))
                  %
                    (kidMVs # map (\x -> makeMetaVar x))
              )
              ( d
                  //
                    (kidSortMVs # map (addSuffix "outer") # map (\x -> x /\ makeMetaVar x))
                  %
                    ( kidMVs `List.zip` rule.kids
                        # map
                            ( \(kidMV /\ { sort }) ->
                                (sort # map V.expand # renameMVs (MV.addPrefix "ch"))
                                  ↓
                                    makeMetaVar kidMV
                            )
                    )
              )
              ( \{ chs, sorts: _, adjs } -> pure
                  { chs: Map.toUnfoldable chs :: List _
                  , sorts: kidSortMVs # map (\x -> (x # addPrefix "outer") /\ (chs MV.!! (x # addPrefix "ch") # outerEndpoint))
                  , adjs: Map.toUnfoldable adjs :: List _
                  }
              )
          ]

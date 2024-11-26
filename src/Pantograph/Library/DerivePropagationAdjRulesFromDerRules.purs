module Pantograph.Library.DerivePropagationAdjRulesFromDerRules where

import Data.Foldable
import Pantograph.Language
import Pantograph.Tree
import Prelude

import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Data.Variant as V
import Pantograph.MetaVar (addPrefix, addSuffix)
import Pantograph.MetaVar as MV

propagationAdjRules
  :: forall d l_d s l_s
   . IsLanguage d s
  => Eq (Variant (SortL s l_s))
  => DerRules d s
  -> AdjRules d l_d s l_s
propagationAdjRules derRules =
  derRules
    # (Map.toUnfoldable :: _ -> List _)
    # foldMap \(d /\ DerRule rule) -> List.fromFoldable
        let
          kidMVs = rule.kids # mapWithIndex \i _ -> MV.MetaVar ("kid_" <> show i)
          -- sortMVs = (rule.sort : (rule.kids # map (\{ sort: s } -> s))) # foldMap collectMVs
          sortMVs = rule.sort # collectMVs
        in
          -- propagate down rules
          [ [ makeAdjRule
                -- freshen each sort metavar in output sort as a new sort change metavar which will be matched against in the down change of the adjust boundary here
                ( (rule.sort # map V.expand # renameMVs (addPrefix "ch")) ↓
                    ( d // (sortMVs # map (\x -> x /\ makeMetaVar x)) %
                        (kidMVs # map (\x -> makeMetaVar x))
                    )
                )
                ( d // (sortMVs # map (\x -> x /\ makeMetaVar (x # addSuffix "outer"))) %
                    ( kidMVs `List.zip` rule.kids
                        # map
                            ( \(kidMV /\ { sort }) ->
                                (sort # map V.expand # renameMVs (MV.addPrefix "ch")) ↓
                                  makeMetaVar kidMV
                            )
                    )
                )
                ( \(AdjSubst { chs, sorts: _, adjs }) -> pure
                    { adjs: Map.toUnfoldable adjs :: List _
                    , chs: chs # Map.toUnfoldable :: List _
                    , sorts: sortMVs # map (\x -> (x # addSuffix "outer") /\ (chs MV.!! (x # addPrefix "ch") # outerEndpoint))
                    }
                )
            ]
          , rule.kids
              # mapWithIndex
                  ( \i _ ->
                      makeAdjRule
                        ( d // (sortMVs # map (\x -> x /\ makeMetaVar x)) %
                            ( kidMVs `List.zip` rule.kids
                                # mapWithIndex
                                    ( \j (kidMV_j /\ { sort: sort_j }) ->
                                        if i == j then
                                          (sort_j # map V.expand # renameMVs (addPrefix "ch")) ↑
                                            makeMetaVar kidMV_j
                                        else
                                          makeMetaVar kidMV_j
                                    )
                            )
                        )
                        ( (rule.sort # map V.expand # renameMVs (MV.addPrefix "ch")) ↑
                            ( d // (sortMVs # map (\x -> x /\ makeMetaVar (x # addSuffix "inner"))) %
                                ( kidMVs `List.zip` rule.kids
                                    # mapWithIndex
                                        ( \j (kidMV_j /\ { sort: sort_j }) ->
                                            if i == j then
                                              makeMetaVar kidMV_j
                                            else
                                              (sort_j # map V.expand # renameMVs (MV.addPrefix "ch")) ↓
                                                makeMetaVar kidMV_j
                                        )
                                )
                            )
                        )
                        ( \(AdjSubst { adjs, chs, sorts: _ }) -> pure
                            { adjs: Map.toUnfoldable adjs :: List _
                            , chs: chs # Map.toUnfoldable :: List _
                            , sorts: sortMVs # map (\x -> (x # addSuffix "inner") /\ (chs MV.!! (x # addPrefix "ch") # outerEndpoint))
                            }
                        )
                  )
              # Array.fromFoldable
          ] # fold

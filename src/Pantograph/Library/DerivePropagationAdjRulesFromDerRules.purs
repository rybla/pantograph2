module Pantograph.Library.DerivePropagationAdjRulesFromDerRules where

import Data.Foldable
import Pantograph.Language
import Pantograph.Tree
import Prelude

import Data.Array as Array
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Pantograph.MetaVar (addPrefix, addSuffix)
import Pantograph.MetaVar as MV
import Pantograph.Utility (expand1')
import Prim.Row (class Lacks)

propagationAdjRules
  :: forall dr sr
   . Lacks "bdry" dr
  => Lacks "metaVar" dr
  => Eq (Variant sr)
  => Eq (ChangeSort sr)
  => Eq (AdjDer dr sr)
  => DerRules dr sr
  -> AdjDerRules dr sr
propagationAdjRules derRules =
  derRules
    # (Map.toUnfoldable :: _ -> List _)
    # foldMap \(d /\ DerRule rule) -> List.fromFoldable
        let
          kidMVs = rule.kids # mapWithIndex \i _ -> MV.MetaVar ("kid_" <> show i)
          sortMVs = rule.sort # collectMVs
        in
          -- propagate down rules
          [ [ makeAdjDerRule
                -- freshen each sort metavar in output sort as a new sort change metavar which will be matched against in the down change of the adjust boundary here
                ( ( rule.sort
                      # map expand_ChangeR
                      # renameMVs (addPrefix "ch")
                  ) ↓
                    ( (d # expand1' @"bdry" >>> expand1' @"metaVar")
                        // (sortMVs <#> \x -> x /\ makeMetaVarSort x)
                        % (kidMVs <#> makeMetaVarDer)
                    )
                )
                ( (d # expand1' @"bdry" >>> expand1' @"metaVar")
                    // (sortMVs <#> \x -> x /\ makeMetaVarSort (x # addSuffix "outer"))
                    %
                      ( kidMVs `List.zip` rule.kids <#> \(kidMV /\ sort) ->
                          ( sort
                              # map expand_ChangeR
                              # renameMVs (MV.addPrefix "ch")
                          ) ↓
                            makeMetaVarDer kidMV
                      )
                )
                \sigma -> do
                  sigma.adjDer # traverseWithIndex_ \x a -> setMetaVar_AdjDer x a
                  sortMVs # traverse_ \x -> setMetaVar_Sort (x # addSuffix "outer") ((sigma.changeSort MV.!! (x # addPrefix "ch")) # _outer)

            ]
          , rule.kids
              # mapWithIndex
                  ( \i _ ->
                      makeAdjDerRule
                        ( (d # expand1' @"bdry" >>> expand1' @"metaVar")
                            // (sortMVs <#> \x -> x /\ makeMetaVarSort x)
                            %
                              ( kidMVs `List.zip` rule.kids
                                  # mapWithIndex
                                      ( \j (kidMV_j /\ sort_j) ->
                                          if i == j then
                                            (sort_j # map expand_ChangeR # renameMVs (addPrefix "ch")) ↑
                                              makeMetaVarDer kidMV_j
                                          else
                                            makeMetaVarDer kidMV_j
                                      )
                              )
                        )
                        ( (rule.sort # map expand_ChangeR # renameMVs (MV.addPrefix "ch")) ↑
                            ( (d # expand1' @"bdry" >>> expand1' @"metaVar")
                                // (sortMVs # map (\x -> x /\ makeMetaVarSort (x # addSuffix "inner")))
                                %
                                  ( kidMVs `List.zip` rule.kids
                                      # mapWithIndex
                                          ( \j (kidMV_j /\ sort_j) ->
                                              if i == j then
                                                makeMetaVarDer kidMV_j
                                              else
                                                (sort_j # map expand_ChangeR # renameMVs (MV.addPrefix "ch")) ↓
                                                  makeMetaVarDer kidMV_j
                                          )
                                  )
                            )
                        )
                        \sigma -> do
                          sigma.adjDer # traverseWithIndex_ \x a -> setMetaVar_AdjDer x a
                          sortMVs # traverse_ \x -> setMetaVar_Sort (x # addSuffix "inner") ((sigma.changeSort MV.!! (x # addPrefix "ch")) # innerEndpoint)
                  )
              # Array.fromFoldable
          ] # fold

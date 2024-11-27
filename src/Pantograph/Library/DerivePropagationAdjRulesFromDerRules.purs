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
import Pantograph.Utility (expand1, todo, unsafeCoerce_because)
import Prim.Row (class Lacks, class Nub)
import Type.Prelude (Proxy(..))

propagationAdjRules
  :: forall dr sr
   . Lacks "bdry" dr
  => Lacks "metaVar" dr
  => DerRules dr sr
  -> AdjDerRules dr sr
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
          [ [ makeAdjDerRule
                -- freshen each sort metavar in output sort as a new sort change metavar which will be matched against in the down change of the adjust boundary here
                ( ( rule.sort
                      # map (expand1 (Proxy :: Proxy "minus") >>> expand1 (Proxy :: Proxy "plus") >>> expand1 (Proxy :: Proxy "replace"))
                      # renameMVs (addPrefix "ch")
                  ) ↓
                    ( (d # expand1 (Proxy :: Proxy "bdry") >>> expand1 (Proxy :: Proxy "metaVar"))
                        // (sortMVs <#> \x -> x /\ makeMetaVarSort x)
                        % (kidMVs <#> makeMetaVarDer)
                    )
                )
                -- ( ?d // (sortMVs # map (\x -> x /\ makeMetaVarSort (x # addSuffix "outer"))) %
                --     ( kidMVs `List.zip` rule.kids
                --         # map
                --             ( \(kidMV /\ { sort }) ->
                --                 (sort # map V.expand # renameMVs (MV.addPrefix "ch")) ↓
                --                   makeMetaVarDer kidMV
                --             )
                --     )
                -- )
                (todo "")
                -- ( \(AdjSubst { chs, sorts: _, adjs }) -> pure
                --     { adjs: Map.toUnfoldable adjs :: List _
                --     , chs: chs # Map.toUnfoldable :: List _
                --     , sorts: sortMVs # map (\x -> (x # addSuffix "outer") /\ (chs MV.!! (x # addPrefix "ch") # outerEndpoint))
                --     }
                -- )
                (todo "")
            ]
          , rule.kids
              # mapWithIndex
                  ( \i _ ->
                      makeAdjDerRule
                        -- ( ?d // (sortMVs # map (\x -> x /\ makeMetaVarSort x)) %
                        --     ( kidMVs `List.zip` rule.kids
                        --         # mapWithIndex
                        --             ( \j (kidMV_j /\ { sort: sort_j }) ->
                        --                 if i == j then
                        --                   (sort_j # map V.expand # renameMVs (addPrefix "ch")) ↑
                        --                     makeMetaVarDer kidMV_j
                        --                 else
                        --                   makeMetaVarDer kidMV_j
                        --             )
                        --     )
                        -- )
                        (todo "")
                        -- ( (rule.sort # map V.expand # renameMVs (MV.addPrefix "ch")) ↑
                        --     ( ?d // (sortMVs # map (\x -> x /\ makeMetaVarSort (x # addSuffix "inner"))) %
                        --         ( kidMVs `List.zip` rule.kids
                        --             # mapWithIndex
                        --                 ( \j (kidMV_j /\ { sort: sort_j }) ->
                        --                     if i == j then
                        --                       makeMetaVarDer kidMV_j
                        --                     else
                        --                       (sort_j # map V.expand # renameMVs (MV.addPrefix "ch")) ↓
                        --                         makeMetaVarDer kidMV_j
                        --                 )
                        --         )
                        --     )
                        -- )
                        (todo "")
                        -- ( \(AdjSubst { adjs, chs, sorts: _ }) -> pure
                        --     { adjs: Map.toUnfoldable adjs :: List _
                        --     , chs: chs # Map.toUnfoldable :: List _
                        --     , sorts: sortMVs # map (\x -> (x # addSuffix "inner") /\ (chs MV.!! (x # addPrefix "ch") # outerEndpoint))
                        --     }
                        -- )
                        (todo "")
                  )
              # Array.fromFoldable
          ] # fold

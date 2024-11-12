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
import MetaVar (MetaVar, (!!))
import MetaVar as MetaVar
import SuperType (inject)

-- propagationAdjRules :: forall d s. IsLanguage d s => AdjRules d s
-- propagationAdjRules =
--   (derRules :: Map d (DerRule s))
--     # (Map.toUnfoldable :: _ -> List _)
--     # foldMap \(d /\ DerRule { sort, kids }) -> List.fromFoldable
--         let
--           kidMetaVars = kids # mapWithIndex \i _ -> MetaVar.MetaVar ("kid_" <> show i)
--         in
--           -- [ makeAdjRule
--           --     -- freshen each sort metavar in output sort as a new sort change metavar which will be matched against in the down change of the adjust boundary here
--           --     ( ( ( sort # map case _ of
--           --             MetaVar (MetaVar.MetaVar x) -> MetaVar (MetaVar.MetaVar ("chi_" <> x))
--           --             InjMetaL s -> InjMetaL (inject s)

--           --         ) :: Tree (MetaL (ChangeL s))
--           --       )
--           --         ↓ d
--           --         //
--           --           ( -- the sort metavars here are basically redundant, since we'll get all necessary information from the unified sort change metavars matched against in the down change
--           --             kids
--           --               # map (\{ sort: s } -> s)
--           --               # collectMetaVars
--           --               # map (\x -> x /\ ((MetaVar x :: MetaL s) % [])) :: List (MetaVar /\ Tree (MetaL s))
--           --           )
--           --         %
--           --           ( ( kidMetaVars
--           --                 # map makeMetaVarExpr'
--           --             ) :: List (Tree (MetaL (AdjL d (MetaL (ChangeL s)) (MetaL s))))
--           --           )
--           --     )
--           --     ( \{ chs, sorts, adjs } -> pure
--           --         { chs: []
--           --         , sorts: []
--           --         , adjs: [] -- kidMetaVars # map \x -> x /\ (?a ↓ (adjs !! x))
--           --         }
--           --     )
--           --     ( d
--           --         //
--           --           ( kids
--           --               # map (\{ sort: s } -> s)
--           --               # collectMetaVars
--           --               # map (\x -> x /\ ((MetaVar x :: MetaL s) % [])) :: List (MetaVar /\ Tree (MetaL s))
--           --           )
--           --         %
--           --           ( ( kidMetaVars
--           --                 # map makeMetaVarExpr'
--           --             ) :: List (Tree (MetaL (AdjL d (MetaL (ChangeL s)) (MetaL s))))
--           --           )
--           --     )
--           -- ]
--           []

-- collectMetaVars =
--   map
--     ( \x -> x # foldMap case _ of
--         InjMetaL l -> Nil
--         MetaVar x -> pure x
--     )
--     >>> fold
--     >>> Set.fromFoldable
--     >>> Set.toUnfoldable
module Test.Slc where

import Pantograph.Example.Slc
import Pantograph.Language
import Pantograph.Tree
import Prelude hiding (zero)

import Control.Monad.Error.Class (class MonadThrow)
import Data.Foldable (intercalate)
import Data.List (List)
import Debug as Debug
import Effect.Exception (Error)
import Pantograph.Library.DerivePropagationAdjRulesFromDerRules (propagationAdjRules)
import Pantograph.Pretty (pretty)
import Test.Common (it_shouldEqual)
import Test.Spec (SpecT, describe)

spec :: forall m g. MonadThrow Error g => Monad m => SpecT g Unit m Unit
spec = describe "Slc" do
  Debug.traceM $ "adjRules: " <> ((propagationAdjRules :: List (AdjRule D S)) # map pretty # intercalate "\n" # ("\n" <> _))
  describe "outerEndpoint" do
    -- if true then do
    --   it_shouldEqual @(SortT S) "xxx"
    --     ((Ext ^% [ Ext %- [] << (Ext ^% [ Emp ^% [] ]) >> [] ]) # outerEndpoint)
    --     (Ext ^% [ Ext ^% [ Emp ^% [] ] ])
    -- else pure unit

    -- describe "propagate" do
    --   if false then do
    --     it_shouldEqual_propagate
    --       (refVarN 1 0 # pure)
    --       (refVarN 1 0)
    --     it_shouldEqual_propagate
    --       (refVarN 2 1 # pure)
    --       (refVarN 2 1)
    --     it_shouldEqual_propagate
    --       (refFreeN 0 0 # pure)
    --       (term (Ext %- [] << ctxN 0 >> []) ↓ refVarN 1 0)
    --     it_shouldEqual_propagate
    --       (freeN 1 0 # pure)
    --       (var (Ext %- [] << ctxN 1 >> []) ↓ varN 2 0)
    --     it_shouldEqual_propagate
    --       (refFreeN 1 0 # pure)
    --       (term (Ext %- [] << ctxN 1 >> []) ↓ refVarN 2 0)
    --     it_shouldEqual_propagate
    --       (freeN 2 1 # pure)
    --       (var (ext $ Ext %- [] << ctxN 1 >> []) ↓ varN 3 1)
    --     it_shouldEqual_propagate
    --       (refFreeN 2 1 # pure)
    --       (term (ext $ Ext %- [] << ctxN 1 >> []) ↓ refVarN 3 1)
    --     it_shouldEqual_propagate
    --       ((lam (ctxN 1) $ refFreeN 2 1) # pure)
    --       (term (Ext %- [] << ctxN 1 >> []) ↓ lam (ctxN 1) (refVarN 3 1))
    --     it_shouldEqual_propagate
    --       (varN 1 0 # pure)
    --       (var (Ext %+ [] << ctxN 0 >> []) ↓ freeN 0 0)
    --   else pure unit

    if true then do
      pure unit
    else pure unit

  -- it_shouldEqual_propagateFixpoint
  --   -- (DerLbl Lam (Map.fromFoldable [ RulialVar "gamma" /\ (Empty %^ []) ]) %* [ ?b ])
  --   (lam emp (ref (ext emp) ?a))
  --   (RightF (DerLbl Lam ?a) %* [])

  -- it_shouldEqual_propagateFixpoint
  --   (lam emp (ref (ext emp) (zero emp)))
  --   (lam_p emp (ref_p (ext emp) (zero_p emp)))

  -- it_shouldEqual_propagateFixpoint
  --   (ref (ext emp) (zero emp))
  --   (term_c (ext_0 %∂- id emp) ↓ ref_p (ext emp) (zero_p emp))

  -- when false $ it_shouldEqual_propagateFixpoint
  --   (ref (ext (ext emp)) (suc (ext (ext emp)) (zero emp)))
  --   (term_c (ext_0 %∂+ id (ext emp)) ↓ ref_p (ext emp) (zero_p emp))

  -- it "ch_ref_1^-1 ∘ gamma ∘ ch_ref_1" do
  --   let gamma = SortLbl Emp %* []
  --   -- let ch_ref_1 = (SortLbl Var %* [ gamma ]) %∂~> (SortLbl Term %* [ gamma ])
  --   let ch_ref_1 = var_0 %∂- (term_0 %∂+ id gamma)
  --   Console.log (pretty { "ch_ref_1": ch_ref_1, "invertChange ch_ref_1": invertChange ch_ref_1, xxx: composeChanges (SortLbl Term %∂. [ id gamma ]) ch_ref_1 })
  --   shouldEqual_pretty
  --     ((SortLbl Var %∂. [ id gamma ]) # Just)
  --     (composeChanges (invertChange ch_ref_1) =<< composeChanges (SortLbl Term %∂. [ id gamma ]) ch_ref_1)

  pure unit


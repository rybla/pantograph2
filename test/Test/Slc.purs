module Test.Slc where

import Pantograph.Example.Slc
import Pantograph.Language
import Prelude hiding (zero)

import Control.Monad.Error.Class (class MonadThrow)
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error)
import Pantograph.Tree ((%))
import Test.Common (shouldEqual_propagate)
import Test.Spec (SpecT, describe)

spec :: forall m g. Monad m => Applicative g => MonadThrow Error g => SpecT g Unit m Unit
spec = describe "Slc" do
  when true do
    shouldEqual_propagate
      (refVarN 1 0 # pure)
      (refVarN 1 0)
    shouldEqual_propagate
      (refVarN 2 1 # pure)
      (refVarN 2 1)
    shouldEqual_propagate
      -- ((ref (ctxN 0) $ free (ctxN 0)) # pure)
      (refFreeN 0 0 # pure)
      (term (Ext %- [] << ctxN 0 >> []) ↓ refVarN 1 0)
    shouldEqual_propagate
      (free (ctxN 1) # pure)
      (var (Ext %- [] << ctxN 1 >> []) ↓ varN 2 0)
    shouldEqual_propagate
      (refFreeN 1 0 # pure)
      (term (Ext %- [] << ctxN 1 >> []) ↓ refVarN 2 0)
    shouldEqual_propagate
      (freeN 1 1 # pure)
      (var (ext $ Ext %- [] << ctxN 1 >> []) ↓ varN 2 1)
  when true do
    -- shouldEqual_propagate
    --   ((lam (ctxN 0) $ refFreeN (ctxN 1) 1) # pure)
    --   (term (Ext %- [] << ctxN 0 >> []) ↓ (lam (ctxN 1) $ refVarN (ctxN 2) 1))
    {-
    -- this shouldn't happen since S[EE∅] can't rewrite to S[∅] by this step!
    -- _should_ rewrite to S[E∅] (only removing the single E)
    {{ Var E-{E{∅}} ↓ S[EE∅]Z[EEE∅] }} ~~>
    S[∅]{{ Var -{E{∅}} ↓ Z[EEE∅] }}
    -}

    pure unit

  -- shouldEqual_propagateFixpoint
  --   -- (DerLbl Lam (Map.fromFoldable [ RulialVar "gamma" /\ (Empty %^ []) ]) %* [ ?b ])
  --   (lam emp (ref (ext emp) ?a))
  --   (RightF (DerLbl Lam ?a) %* [])

  -- shouldEqual_propagateFixpoint
  --   (lam emp (ref (ext emp) (zero emp)))
  --   (lam_p emp (ref_p (ext emp) (zero_p emp)))

  -- shouldEqual_propagateFixpoint
  --   (ref (ext emp) (zero emp))
  --   (term_c (ext_0 %∂- id emp) ↓ ref_p (ext emp) (zero_p emp))

  -- when false $ shouldEqual_propagateFixpoint
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


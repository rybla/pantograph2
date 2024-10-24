module Test.Slc where

-- import Pantograph.Example.Slc
import Pantograph.Language
import Pantograph.Propagation
import Pantograph.Tree
import Prelude hiding (zero)

import Data.Maybe (Maybe(..))
import Effect.Class.Console as Console
import Pantograph.Pretty (pretty)
import Test.Common (shouldEqual_pretty)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = describe "Slc" do
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
  --   let gamma = SortLabel Emp %* []
  --   -- let ch_ref_1 = (SortLabel Var %* [ gamma ]) %∂~> (SortLabel Term %* [ gamma ])
  --   let ch_ref_1 = var_0 %∂- (term_0 %∂+ id gamma)
  --   Console.log (pretty { "ch_ref_1": ch_ref_1, "invertChange ch_ref_1": invertChange ch_ref_1, xxx: composeChanges (SortLabel Term %∂. [ id gamma ]) ch_ref_1 })
  --   shouldEqual_pretty
  --     ((SortLabel Var %∂. [ id gamma ]) # Just)
  --     (composeChanges (invertChange ch_ref_1) =<< composeChanges (SortLabel Term %∂. [ id gamma ]) ch_ref_1)

  pure unit

shouldEqual_propagateFixpoint d pd =
  -- it (pretty pd) $
  --   shouldEqual_pretty
  --     d
  --     (fromPropagDerivToDeriv $ propagateFixpoint propagRules $ pd)

  pure unit


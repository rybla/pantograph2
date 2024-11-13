module Test.Slc where

-- import Pantograph.Example.Slc
-- import Pantograph.Language
-- import Pantograph.Propagation
-- import Pantograph.Tree
import Prelude hiding (zero)

-- import Data.Map as Map
-- import Data.Maybe (Maybe(..))
-- import Data.Tuple.Nested ((/\))
-- import Effect.Aff (Error)
-- import Effect.Class.Console as Console
-- import Pantograph.EitherF (EitherF(..))
-- import Pantograph.Pretty (pretty)
-- import Test.Common (shouldEqual_pretty)
-- import Test.Spec (Spec, describe, it)
import Test.Spec (Spec, describe)

-- emp = Empty %^ []
-- ext g = Ext %^ [ g ]
-- lam g b = DerLbl Lam (Map.fromFoldable [ RulialVar "gamma" /\ g ]) %* [ b ]
-- ref g x = DerLbl Ref (Map.fromFoldable [ RulialVar "gamma" /\ g ]) %* [ x ]

spec :: Spec Unit
spec = describe "Slc" do
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

-- shouldEqual_propagateFixpoint
--   :: forall d s
--    . IsAdjustLanguage d s
--   => Tree (DerLbl d s)
--   -> Tree (AdjustLbl d s)
--   -> Spec Unit
-- shouldEqual_propagateFixpoint d pd = do
--   it (pretty pd) $
--     shouldEqual_pretty
--       d
--       (fromAdjustDerivToDeriv $ propagateFixpoint adjustRules $ pd)
--   pure unit

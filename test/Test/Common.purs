module Test.Common where

import Pantograph.Language
import Pantograph.Tree
import Prelude

import Data.Foldable (fold, intercalate)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Pantograph.Pretty (indent, pretty)
import Pantograph.Propagation (propagate)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (fail)

also_show = true

shouldEqual_pretty expected actual =
  unless (expected == actual) do
    fail
      ( [ [ "expected"
          , ""
          , pretty expected
          ]
        , if also_show then
            [ ""
            , show expected
            ]
          else []
        , [ ""
          , "but actually got"
          , ""
          , pretty actual
          ]
        , if also_show then
            [ ""
            , show actual
            ]
          else []
        , []
        ]
          # fold
          # intercalate "\n"
          # indent 1
      )

-- shouldEqual_propagate
--   :: forall d s
--    . IsAdjLanguage d s
--   => TreeV (AdjL (ChangeL (SortL s ())) (DerL d (SortL s ()) ()))
--   -> TreeV (AdjL (ChangeL (SortL s ())) (DerL d (SortL s ()) ()))
--   -> Spec Unit
-- shouldEqual_propagate d pd = do
--   it (pretty pd) $
--     shouldEqual_pretty
--       d
--       -- (fromAdjustDerivToDeriv $ propagateFixpoint adjustRules $ pd)
--       (propagate ?a # (unwrap :: Identity _ -> _))
--   pure unit

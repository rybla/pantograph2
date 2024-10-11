module Test.Common where

import Prelude

import Data.Foldable (intercalate)
import Pantograph.Pretty (indent, pretty)
import Test.Spec.Assertions (fail)

shouldEqual_pretty expected actual =
  unless (expected == actual) do
    fail
      ( [ "expected"
        , ""
        , pretty expected
        , ""
        , "but got"
        , ""
        , pretty actual
        ]
          # intercalate "\n"
          # indent 1
      )

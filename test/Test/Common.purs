module Test.Common where

import Prelude

import Data.Foldable (fold, intercalate)
import Pantograph.Pretty (indent, pretty)
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

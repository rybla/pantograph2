module Test.Common where

import Pantograph.Language
import Pantograph.Tree
import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Writer (runWriter)
import Data.Either (either)
import Data.Either.Nested (type (\/))
import Data.Foldable (fold, intercalate)
import Data.Identity (Identity)
import Data.List ((:))
import Data.Newtype (unwrap)
import Data.Newtype (unwrap)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Pantograph.Debug (logM)
import Pantograph.Pretty (class Pretty, indent, pretty)
import Pantograph.Propagation (propagate)
import Test.Spec (class Example, Spec, SpecT, it)
import Test.Spec.Assertions (fail, shouldEqual)

also_show = false :: Boolean

-- shouldEqual_pretty :: forall m a. Eq a => MonadThrow Error m => Pretty a => Show a => a -> a -> m Unit
shouldEqual_pretty expected actual =
  unless (expected == actual) do
    fail
      ( [ [ "expected"
          , ""
          , "    " <> pretty expected
          ]
        , if also_show then
            [ ""
            , "    " <> show expected
            ]
          else []
        , [ ""
          , "but actually got"
          , ""
          , "    " <> pretty actual
          ]
        , if also_show then
            [ ""
            , "    " <> show actual
            ]
          else []
        , []
        ]
          # fold
          # intercalate "\n"
          # indent 1
      )

it_shouldEqual_propagate d pd = do
  it (pretty pd) do
    let result /\ logs = propagate pd # runWriter
    logM 0 $
      [ "propagation steps:" # pure
      , (pd : logs)
          # map (\adj -> "  - " <> pretty adj)
      ]
        # fold
        # intercalate "\n"
    shouldEqual_pretty d result

it_shouldEqual
  :: forall g m @a
   . Eq a
  => Pretty a
  => Show a
  => MonadThrow Error g
  => Monad m
  => String
  -> a
  -> a
  -> SpecT g Unit m Unit
it_shouldEqual name a a' = it name (shouldEqual_pretty a a')

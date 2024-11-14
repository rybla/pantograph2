module Test.Common where

import Pantograph.Language
import Pantograph.Tree
import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Writer (runWriter)
import Data.Either (either)
import Data.Either.Nested (type (\/))
import Data.Foldable (fold, intercalate)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Data.Newtype (unwrap)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error)
import Pantograph.Pretty (class Pretty, indent, pretty)
import Pantograph.Propagation (propagate)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (fail)

also_show = true :: Boolean

shouldEqual_pretty :: forall m a. Eq a => MonadThrow Error m => Pretty a => Show a => a -> a -> m Unit
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

shouldEqual_propagate :: forall d s. IsAdjLanguage d s => AdjT d s \/ DerT d s -> AdjT d s -> Spec Unit
shouldEqual_propagate d pd = do
  it (pretty pd) do
    shouldEqual_pretty d (propagate pd # runWriter # fst)

module Pantograph.Library.Cursor where

import Pantograph.Language
import Pantograph.Tree
import Prelude

import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Data.Variant as V
import Pantograph.MetaVar as MV
import Pantograph.Utility (bug, emptyRecordOfMaps, todo)
import Type.Proxy (Proxy(..))

type CursorR dr = (cursor :: Cursor | dr)

_cursor = Proxy :: Proxy "cursor"

data Cursor
  = Point
  | SelectOuter
  | SelectInner

derive instance Generic Cursor _

instance Show Cursor where
  show x = genericShow x

derive instance Eq Cursor

derive instance Ord Cursor

instance PrettyTreeL Cursor where
  prettyTreeL Point (kid : Nil) = "[ " <> kid <> " ]"
  prettyTreeL SelectOuter (kid : Nil) = "[<[ " <> kid <> " ]>]"
  prettyTreeL SelectInner (kid : Nil) = "[>[ " <> kid <> " ]<]"
  prettyTreeL _ _ = bug "invalid cursor kids"

point :: forall dr sr. Der (CursorR dr) sr -> Der (CursorR dr) sr
point kid = DerL (V.inj _cursor Point) Map.empty % [ kid ]

selectOuter kid = DerL (V.inj _cursor SelectOuter) Map.empty % [ kid ]
selectInner kid = DerL (V.inj _cursor SelectInner) Map.empty % [ kid ]

adjRules :: forall dr sr. AdjDerRules (CursorR dr) sr
adjRules = List.fromFoldable
  ( [ makePassthroughs point
    , makePassthroughs selectOuter
    , makePassthroughs selectInner
    ] # fold
  )
  where
  _ch /\ ch = defAndMakeMetaVarSort "ch"
  _kid /\ kid = defAndMakeMetaVarDer "kid"

  makePassthroughs wrap = [ makePassthroughDown wrap, makePassthroughUp wrap ]
  makePassthroughDown wrap =
    makeAdjDerRule
      (ch ↓ (wrap kid))
      (wrap (ch ↓ kid))
      (\sigma -> pure { adjDer: [ _kid # MV.id sigma.adjDer ], changeSort: [ _ch # MV.id sigma.changeSort ], sort: [] })
  makePassthroughUp wrap =
    makeAdjDerRule
      (wrap (ch ↑ kid))
      (ch ↑ (wrap kid))
      (\sigma -> pure { adjDer: [ _kid # MV.id sigma.adjDer ], changeSort: [ _ch # MV.id sigma.changeSort ], sort: [] })


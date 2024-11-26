module Pantograph.Library.Cursor where

import Pantograph.Language
import Pantograph.Tree
import Prelude

import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Data.Variant as V
import Pantograph.MetaVar as MV
import Pantograph.Utility (bug)
import Type.Proxy (Proxy(..))

type CursorL l_d = (cursor :: Cursor | l_d)

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

point kid = V.inj _cursor Point % [ kid ]
selectOuter kid = V.inj _cursor SelectOuter % [ kid ]
selectInner kid = V.inj _cursor SelectInner % [ kid ]

adjRules :: forall d l_d s l_s. AdjRules d (CursorL l_d) s l_s
adjRules = List.fromFoldable
  ( [ makePassthroughs point
    , makePassthroughs selectOuter
    , makePassthroughs selectInner
    ] # fold
  )
  where
  _ch /\ ch = defAndMakeMetaVar "ch"
  _kid /\ kid = defAndMakeMetaVar "kid"

  makePassthroughs wrap = [ makePassthroughDown wrap, makePassthroughUp wrap ]
  makePassthroughDown wrap =
    makeAdjRule (ch ↓ (wrap kid)) (wrap (ch ↓ kid))
      (\(AdjSubst { adjs, chs }) -> pure { adjs: [ MV.id adjs _kid ], chs: [ MV.id chs _ch ], sorts: [] })
  makePassthroughUp wrap =
    makeAdjRule (wrap (ch ↑ kid)) (ch ↑ (wrap kid))
      (\(AdjSubst { adjs, chs }) -> pure { adjs: [ MV.id adjs _kid ], chs: [ MV.id chs _ch ], sorts: [] })


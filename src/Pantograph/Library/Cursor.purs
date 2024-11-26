module Pantograph.Library.Cursor where

import Pantograph.Language
import Pantograph.Tree
import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Show.Generic (genericShow)
import Pantograph.Utility (bug)

type CursorL l_d = (cursor :: Cursor | l_d)

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

-- adjRules :: AdjRules d s 

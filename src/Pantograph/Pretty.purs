module Pantograph.Pretty where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Prim.Row (class Nub)
import Prim.RowList as RL
import Record.Unsafe (unsafeGet)
import Type.Prelude (Proxy(..))

class Pretty a where
  pretty :: a -> String

instance Pretty Int where
  pretty x = show x

instance Pretty Boolean where
  pretty x = show x

instance Pretty Number where
  pretty x = show x

instance Pretty String where
  pretty x = show x

instance Pretty a => Pretty (Array a) where
  pretty x = "[" <> (x # map pretty # String.joinWith ", ") <> "]"

instance Pretty a => Pretty (List a) where
  pretty x = "[" <> (x # map pretty # Array.fromFoldable # String.joinWith ", ") <> "]"

instance Pretty a => Pretty (Set a) where
  pretty x = "{" <> (x # Set.map pretty # Array.fromFoldable # String.joinWith ", ") <> "}"

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = "Nothing"
  pretty (Just x) = "Just " <> pretty x

instance (Pretty e, Pretty a) => Pretty (Either e a) where
  pretty (Left x) = pretty x
  pretty (Right x) = pretty x

instance (Pretty e, Pretty a) => Pretty (Tuple e a) where
  pretty (x /\ y) = pretty x <> ", " <> pretty y

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty = (Map.toUnfoldable :: _ -> Array _) >>> map (\(k /\ v) -> pretty k <> " ↦ " <> pretty v) >>> intercalate ", " >>> braces

instance Pretty (a -> b) where
  pretty _ = "<function>"

instance prettyRecord ::
  ( Nub rs rs
  , RL.RowToList rs ls
  , PrettyRecordFields ls rs
  ) =>
  Pretty (Record rs) where
  pretty record = "[record]" <> prettyRecordFields (Proxy :: Proxy ls) record

-- | A class for records where all fields have `Pretty` instances, used to
-- | implement the `Pretty` instance for records.
class PrettyRecordFields :: RL.RowList Type -> Row Type -> Constraint
class PrettyRecordFields rowlist row where
  prettyRecordFields :: Proxy rowlist -> Record row -> String

instance prettyRecordFieldsNil :: PrettyRecordFields RL.Nil row where
  prettyRecordFields _ _ = ""
else instance prettyRecordFieldsConsNil ::
  ( IsSymbol key
  , Pretty focus
  ) =>
  PrettyRecordFields (RL.Cons key focus RL.Nil) row where
  prettyRecordFields _ record = "\n- " <> key <> ": " <> indent 1 (pretty focus) <> " "
    where
    key = reflectSymbol (Proxy :: Proxy key)
    focus = unsafeGet key record :: focus
else instance prettyRecordFieldsCons ::
  ( IsSymbol key
  , PrettyRecordFields rowlistTail row
  , Pretty focus
  ) =>
  PrettyRecordFields (RL.Cons key focus rowlistTail) row where
  prettyRecordFields _ record = "\n- " <> key <> ": " <> indent 1 (pretty focus) <> tail
    where
    key = reflectSymbol (Proxy :: Proxy key)
    focus = unsafeGet key record :: focus
    tail = prettyRecordFields (Proxy :: Proxy rowlistTail) record

indent :: Int -> String -> String
indent i str =
  let
    lines = str # String.split (String.Pattern "\n")
    spaces = "  " # Array.replicate i # Array.fold
  in
    if Array.length lines < 2 then
      str
    else
      lines # Array.foldMap (("\n" <> spaces) <> _)

parens :: String -> String
parens str = "(" <> str <> ")"

brackets :: String -> String
brackets str = "[" <> str <> "]"

braces :: String -> String
braces str = "{" <> str <> "}"


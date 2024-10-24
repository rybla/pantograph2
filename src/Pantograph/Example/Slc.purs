module Pantograph.Example.Slc where

import Pantograph.Language
import Pantograph.Tree
import Prelude

import Control.Alternative (empty)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Show.Generic (genericShow)
import Pantograph.RevList (RevList(..))
import Pantograph.Utility (bug, todo)

data S
  = Emp
  | Ext
  | Var
  | Term

derive instance Generic S _

instance Show S where
  show x = genericShow x

instance PrettyTreeLabel S where
  prettyTree Emp Nil = "∅"
  prettyTree Ext (gamma : Nil) = "E" <> gamma
  prettyTree Var (gamma : Nil) = "Var " <> gamma
  prettyTree Term (gamma : Nil) = "Term " <> gamma
  prettyTree _ _ = bug "invalid S"

data D
  = Zero
  | Suc
  | Ref
  | Lam
  | App

derive instance Generic D _

instance Show D where
  show x = genericShow x

instance PrettyTreeLabel D where
  prettyTree Zero Nil = "Z"
  prettyTree Suc (n : Nil) = "S" <> n
  prettyTree Ref (x : Nil) = "#" <> x
  prettyTree Lam (b : Nil) = "(λ " <> b <> ")"
  prettyTree App (f : a : Nil) = "(" <> f <> " " <> a <> ")"
  prettyTree _ _ = bug "invalid D"

derivRules :: DerivRules D S
derivRules Zero =
  DerivRule
    { name: "Zero"
    , kids: Nil
    , sort: Right (SortLabel Var) % ((Right (SortLabel Ext) % ((Left (RulialVar "gamma") % Nil) : Nil)) : Nil)
    }
derivRules Suc =
  DerivRule
    { name: "Suc"
    , kids: List.fromFoldable
        [ { sort: Right (SortLabel Var) % ((Left (RulialVar "gamma") % Nil) : Nil)
          , passthrough_down: case _ of
              Congruence (SortLabel Var) % ((Congruence (SortLabel Ext) % (ch_gamma : Nil)) : Nil) -> pure $
                Congruence (SortLabel Var) % (ch_gamma : Nil)
              _ -> empty
          , passthrough_up: case _ of
              Congruence (SortLabel Var) % (ch_gamma : Nil) -> pure $
                Congruence (SortLabel Var) % ((Congruence (SortLabel Ext) % (ch_gamma : Nil)) : Nil)
              _ -> empty
          , wrap_down: case _ of
              Congruence (SortLabel Var) % ((Plus (Tooth (SortLabel Ext) (RevList Nil) Nil) % (ch_gamma : Nil)) : Nil) -> pure
                { up: id $ SortLabel Var % ((SortLabel Ext % ((ch_gamma # outerEndpoint) : Nil)) : Nil)
                , down: Congruence (SortLabel Var) % (ch_gamma : Nil)
                }
              _ -> empty
          , wrap_up: case _ of
              Congruence (SortLabel Var) % ((Minus (Tooth (SortLabel Ext) (RevList Nil) Nil) % (ch_gamma : Nil)) : Nil) -> pure
                { up: Congruence (SortLabel Var) % ((Congruence (SortLabel Ext) % (ch_gamma : Nil)) : Nil)
                , down: id $ SortLabel Var % ((ch_gamma # innerEndpoint) : Nil)
                }
              _ -> empty
          , unwrap_down: case _ of
              Congruence (SortLabel Var) % ((Minus (Tooth (SortLabel Ext) (RevList Nil) Nil) % (ch_gamma : Nil)) : Nil) -> pure
                { up: id $ SortLabel Var % ((ch_gamma # outerEndpoint) : Nil)
                , down: Congruence (SortLabel Var) % (ch_gamma : Nil)
                }
              _ -> empty
          , unwrap_up: case _ of
              Congruence (SortLabel Var) % ((Plus (Tooth (SortLabel Ext) (RevList Nil) Nil) % (ch_gamma : Nil)) : Nil) -> pure
                { up: Congruence (SortLabel Var) % (ch_gamma : Nil)
                , down: id $ SortLabel Var % ((ch_gamma # innerEndpoint) : Nil)
                }
              _ -> empty
          }
        ]
    , sort: Right (SortLabel Var) % ((Right (SortLabel Ext) % ((Left (RulialVar "gamma") % Nil) : Nil)) : Nil)
    }
derivRules Ref = todo ""
derivRules Lam = todo ""
derivRules App = todo ""


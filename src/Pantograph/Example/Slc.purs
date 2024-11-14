{-
one major limitation here is that you can't re-nest lambdas and have the
referenced variables recover -- since there isn't a way to tell that the
re-nested lambda corresponds to the new position that the free variables should
take.
-}
module Pantograph.Example.Slc where

import Pantograph.Language
import Pantograph.Tree
import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), intercalate, (:))
import Data.List as List
import Data.Map as Map
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Pantograph.MetaVar ((!!))
import Pantograph.Library.DerivePropagationAdjRulesFromDerRules (propagationAdjRules)
import Pantograph.Utility (bug)

data S
  = Emp
  | Ext
  | Var
  | Term

derive instance Generic S _

instance Show S where
  show x = genericShow x

instance PrettyTreeL S where
  prettyTreeL Emp Nil = "∅"
  prettyTreeL Ext (g : Nil) = "E" <> g
  prettyTreeL Var (g : Nil) = "Var " <> g
  prettyTreeL Term (g : Nil) = "Term " <> g
  prettyTreeL s ss = bug $ "invalid S: " <> show s <> "(" <> (ss # intercalate ", ") <> ")"

instance Eq S where
  eq x = genericEq x

instance Ord S where
  compare x = genericCompare x

instance IsSortL S

data D
  = Free
  | Zero
  | Suc
  | Ref
  | Lam
  | App
  | Hole

derive instance Generic D _

instance Show D where
  show x = genericShow x

instance Eq D where
  eq x = genericEq x

instance Ord D where
  compare x = genericCompare x

instance PrettyTreeL D where
  prettyTreeL Free Nil = "F"
  prettyTreeL Zero Nil = "Z"
  prettyTreeL Suc (n : Nil) = "S" <> n
  prettyTreeL Ref (x : Nil) = "#" <> x
  prettyTreeL Lam (b : Nil) = "(λ " <> b <> ")"
  prettyTreeL App (f : a : Nil) = "(" <> f <> " " <> a <> ")"
  prettyTreeL Hole Nil = "?"
  prettyTreeL d ss = bug $ "invalid D: " <> show d <> "(" <> (ss # intercalate ", ") <> ")"

instance IsDerL D

instance HasDerRules D S where
  derRules = Map.fromFoldable
    [ Free /\
        ( []
            |- (Var %^ [ g ])
        )
    , Zero /\
        ( [] |-
            (Var %^ [ Ext %^ [ g ] ])
        )
    , Suc /\
        ( [ Var %^ [ g ] ] |-
            (Var %^ [ Ext %^ [ g ] ])
        )
    , Ref /\
        ( [ Var %^ [ g ] ] |-
            (Term %^ [ g ])
        )
    , Lam /\
        ( [ Term %^ [ Ext %^ [ g ] ] ] |-
            (Term %^ [ g ])
        )
    , App /\
        ( [ Term %^ [ g ]
          , Term %^ [ g ]
          ] |-
            (Term %^ [ g ])
        )
    , Hole /\
        ( [] |-
            (Term %^ [ g ])
        )
    ]
    where
    g = makeMetaVar' "g"

instance IsLanguage D S

instance HasAdjRules D S where
  adjRules = modifyAdjRules <> propagationAdjRules
    where
    _g /\ g = defAndMakeMetaVar "g"
    _g' /\ g' = defAndMakeMetaVar "g'"
    _dg /\ dg = defAndMakeMetaVar "dg"

    modifyAdjRules = List.fromFoldable
      [ makeAdjRule
          (Var %^ [ Ext %- [] << dg >> [] ] ↓ Zero // [ _g /\ g ] % [])
          (Free // [ _g /\ g' ] % [])
          ( \{ sorts: _, chs, adjs: _ } ->
              pure { sorts: [ _g' /\ (chs !! _dg # outerEndpoint) ], chs: [], adjs: [] }
          )
      , makeAdjRule
          (Var %^ [ Ext %+ [] << dg >> [] ] ↓ Free // [ _g /\ g ] % [])
          (Zero // [ _g /\ g' ] % [])
          ( \{ sorts: _, chs, adjs: _ } ->
              pure { sorts: [ _g' /\ (chs !! _dg # outerEndpoint) ], chs: [], adjs: [] }
          )
      ]

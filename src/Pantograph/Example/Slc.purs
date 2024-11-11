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
import Data.List as List
import Data.Map as Map
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import MetaVar (getMetaVar, (!!))
import Type.Proxy (Proxy(..))

data S
  = Emp
  | Ext
  | Var
  | Term

derive instance Generic S _

instance Show S where
  show x = genericShow x

-- instance PrettyTreeLbl S where
--   prettyTree Emp Nil = "∅"
--   prettyTree Ext (g : Nil) = "E" <> g
--   prettyTree Var (g : Nil) = "Var " <> g
--   prettyTree Term (g : Nil) = "Term " <> g
--   prettyTree _ _ = bug "invalid S"

instance Eq S where
  eq x = genericEq x

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

-- instance PrettyTreeLbl D where
--   prettyTree Free Nil = "F"
--   prettyTree Zero Nil = "Z"
--   prettyTree Suc (n : Nil) = "S" <> n
--   prettyTree Ref (x : Nil) = "#" <> x
--   prettyTree Lam (b : Nil) = "(λ " <> b <> ")"
--   prettyTree App (f : a : Nil) = "(" <> f <> " " <> a <> ")"
--   prettyTree Hole Nil = "?"
--   prettyTree _ _ = bug "invalid D"

instance HasDerRules D S where
  derRules = Map.fromFoldable
    [ Free /\
        ( []
            |- (Var % [ g ])
        )
    , Zero /\
        ( [] |-
            (Var % [ Ext % [ g ] ])
        )
    , Suc /\
        ( [ Var % [ g ] ] |-
            (Var % [ Ext % [ g ] ])
        )
    , Ref /\
        ( [ Var % [ g ] ] |-
            (Term % [ g ])
        )
    , Lam /\
        ( [ Term % [ Ext % [ g ] ] ] |-
            (Term % [ g ])
        )
    , App /\
        ( [ Term % [ g ]
          , Term % [ g ]
          ] |-
            (Term % [ g ])
        )
    , Hole /\
        ( [] |-
            (Term % [ g ])
        )
    ]
    where
    g = makeMetaVarExpr "g"

instance HasAdjRules D S where
  adjRules = modifyAdjRules
    where
    _g /\ g = makeMetaVarAndExpr "g" (Proxy :: Proxy S)
    _g' /\ g' = makeMetaVarAndExpr "g'" (Proxy :: Proxy S)
    _dg /\ dg = makeMetaVarAndExpr "dg" (Proxy :: Proxy (ChangeL S))

    modifyAdjRules = List.fromFoldable
      [ makeAdjRule
          (Var % [ Ext %- [] << dg >> [] ] ↓ Zero // [ _g /\ g ] % [])
          ( \sigma@{ sorts, changes } -> pure sigma
              { sorts = sorts # Map.insert _g' (changes !! _dg # outerEndpoint) }
          )
          (Free // [ _g /\ g' ] % [])
      , makeAdjRule
          (Var % [ Ext %+ [] << dg >> [] ] ↓ Free // [ _g /\ g ] % [])
          ( \sigma -> pure sigma
              { sorts = sigma.sorts #
                  Map.insert _g (sigma.changes # getMetaVar _dg # outerEndpoint)
              }
          )
          (Zero // [ _g /\ g ] % [])
      ]

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
import Pantograph.Library.DerivePropagationAdjRulesFromDerRules (propagationAdjRules)
import Pantograph.MetaVar ((!!))
import Pantograph.MetaVar as MV
import Pantograph.Pretty (brackets)
import Pantograph.Utility (bug)

--------------------------------------------------------------------------------
-- MetaVars
--------------------------------------------------------------------------------

_g = MV.MetaVar "g"
g = makeMetaVar' "g"

--------------------------------------------------------------------------------
-- S
--------------------------------------------------------------------------------

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

emp = Emp ^% []
ext g = Ext ^% [ g ]
var g = Var ^% [ g ]
term g = Term ^% [ g ]

ctxN n | n < 0 = bug $ "invalid: ctxN; n = " <> show n
ctxN n | n == 0 = emp
ctxN n = ext (ctxN (n - 1))

--------------------------------------------------------------------------------
-- D
--------------------------------------------------------------------------------

data D
  = Free
  | Zero
  | Suc
  | Ref
  | Lam
  | App
  | Hole

free g = Free // [ _g /\ g ] % []
zero g = Zero // [ _g /\ g ] % []
suc g x = Suc // [ _g /\ g ] % [ x ]
ref g x = Ref // [ _g /\ g ] % [ x ]
lam g b = Lam // [ _g /\ g ] % [ b ]
app g f a = App // [ _g /\ g ] % [ f, a ]
hole g = Hole // [ _g /\ g ] % []

varN g n | n < 0 = bug $ "invalid: varN; n = " <> show n
varN g 0 = zero (ctxN (g - 1))
varN g n = suc (ctxN (g - 1)) (varN (g - 1) (n - 1))

refVarN g n = ref (ctxN g) (varN g n)

-- `freeN g n` is the free var with `n` sucs in context `g`
freeN g n | n < 0 = bug $ "invalid: freeN; n = " <> show n
freeN g 0 = free (ctxN g)
freeN g n = suc (ctxN (g - 1)) (freeN (g - 1) (n - 1))

refFreeN g n = ref (ctxN g) (freeN g n)

derive instance Generic D _

instance Show D where
  show x = genericShow x

instance Eq D where
  eq x = genericEq x

instance Ord D where
  compare x = genericCompare x

instance PrettyTreeDerL D where
  prettyTreeDerL Free sigma Nil = "F" <> brackets (sigma MV.!! _g)
  prettyTreeDerL Zero sigma Nil = "Z" <> brackets (sigma MV.!! _g)
  prettyTreeDerL Suc sigma (n : Nil) = "S" <> brackets (sigma MV.!! _g) <> n
  prettyTreeDerL Ref sigma (x : Nil) = "#" <> brackets (sigma MV.!! _g) <> x
  prettyTreeDerL Lam sigma (b : Nil) = "(λ" <> brackets (sigma MV.!! _g) <> " " <> b <> ")"
  prettyTreeDerL App sigma (f : a : Nil) = "(" <> brackets (sigma MV.!! _g) <> " " <> f <> " " <> a <> ")"
  prettyTreeDerL Hole sigma Nil = "?" <> brackets (sigma MV.!! _g)
  prettyTreeDerL d sigma ss = bug $ "invalid D: " <> show d <> "(" <> (ss # intercalate ", ") <> ")"

instance IsDerL D

--------------------------------------------------------------------------------
-- HasDerRules
--------------------------------------------------------------------------------

instance HasDerRules D S where
  derRules = Map.fromFoldable
    [ Free /\
        ( []
            |- (Var ^% [ g ])
        )
    , Zero /\
        ( [] |-
            (Var ^% [ Ext ^% [ g ] ])
        )
    , Suc /\
        ( [ Var ^% [ g ] ] |-
            (Var ^% [ Ext ^% [ g ] ])
        )
    , Ref /\
        ( [ Var ^% [ g ] ] |-
            (Term ^% [ g ])
        )
    , Lam /\
        ( [ Term ^% [ Ext ^% [ g ] ] ] |-
            (Term ^% [ g ])
        )
    , App /\
        ( [ Term ^% [ g ]
          , Term ^% [ g ]
          ] |-
            (Term ^% [ g ])
        )
    , Hole /\
        ( [] |-
            (Term ^% [ g ])
        )
    ]

--------------------------------------------------------------------------------
-- IsLanguage
--------------------------------------------------------------------------------

instance IsLanguage D S

--------------------------------------------------------------------------------
-- HasAdjRules
--------------------------------------------------------------------------------

instance HasAdjRules D S where
  adjRules = modifyAdjRules <> propagationAdjRules
    where
    _g /\ g = defAndMakeMetaVar "g"
    _g' /\ g' = defAndMakeMetaVar "g'"
    _dg /\ dg = defAndMakeMetaVar "dg"

    modifyAdjRules = List.fromFoldable
      -- these really should be the only necessary rules since we don't have types
      [ makeAdjRule
          (Var ^% [ Ext %- [] << dg >> [] ] ↓ Zero // [ _g /\ g ] % [])
          (Free // [ _g /\ g' ] % [])
          ( \(AdjSubst { sorts: _, chs, adjs: _ }) ->
              pure { adjs: [], chs: [], sorts: [ _g' /\ (chs !! _dg # outerEndpoint) ] }
          )
      , makeAdjRule
          (Var ^% [ Ext %+ [] << dg >> [] ] ↓ Free // [ _g /\ g ] % [])
          (Zero // [ _g /\ g' ] % [])
          ( \(AdjSubst { sorts: _, chs, adjs: _ }) ->
              pure { adjs: [], chs: [], sorts: [ _g' /\ (chs !! _dg # outerEndpoint) ] }
          )
      ]

--------------------------------------------------------------------------------
-- IsAdjLanguage
--------------------------------------------------------------------------------

instance IsAdjLanguage D S

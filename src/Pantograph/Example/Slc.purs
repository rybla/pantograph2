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

import Control.Alternative (guard)
import Control.Monad.State (execStateT)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), intercalate, (:))
import Data.List as List
import Data.Map as Map
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Data.Variant as V
import Pantograph.Library.Cursor (CursorR)
import Pantograph.Library.Cursor as Cursor
import Pantograph.Library.DerivePropagationAdjRulesFromDerRules (derive_propagationAdjRules)
import Pantograph.MetaVar ((!!))
import Pantograph.MetaVar as MV
import Pantograph.Pretty (brackets, pretty)
import Pantograph.Utility (bug, emptyRecordOfMaps, todo, (##))
import Prim.Row (class Lacks)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- MetaVars
--------------------------------------------------------------------------------

_g = MV.MetaVar "g"
g = makeMetaVarSort _g
_g' = MV.MetaVar "g'"
g' = makeMetaVarSort _g'
_dg = MV.MetaVar "dg"
dg = makeMetaVarSort _dg
_dg' = MV.MetaVar "dg'"
dg' = makeMetaVarSort _dg'

_d = MV.MetaVar "d"
d = makeMetaVarDer _d

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

emp = Emp .% []
ext g = Ext .% [ g ]
var g = Var .% [ g ]
term g = Term .% [ g ]

ctxN n | n < 0 = bug $ "invalid: ctxN; n = " <> show n
ctxN n | n == 0 = emp
ctxN n = ext (ctxN (n - 1))

--------------------------------------------------------------------------------
-- SR
--------------------------------------------------------------------------------

type SR = (BaseR S ())

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

free g = (Free .// [ _g /\ g ]) % []
zero g = (Zero .// [ _g /\ g ]) % []
suc g x = (Suc .// [ _g /\ g ]) % [ x ]
ref g x = (Ref .// [ _g /\ g ]) % [ x ]
lam g b = (Lam .// [ _g /\ g ]) % [ b ]
app g f a = (App .// [ _g /\ g ]) % [ f, a ]
hole g = (Hole .// [ _g /\ g ]) % []

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

instance PrettyDerL D where
  prettyDerL Free sigma Nil = "F" <> brackets (sigma MV.!! _g)
  prettyDerL Zero sigma Nil = "Z" <> brackets (sigma MV.!! _g)
  prettyDerL Suc sigma (n : Nil) = "S" <> brackets (sigma MV.!! _g) <> n
  prettyDerL Ref sigma (x : Nil) = "#" <> brackets (sigma MV.!! _g) <> x
  prettyDerL Lam sigma (b : Nil) = "(λ" <> brackets (sigma MV.!! _g) <> " " <> b <> ")"
  prettyDerL App sigma (f : a : Nil) = "(" <> brackets (sigma MV.!! _g) <> " " <> f <> " " <> a <> ")"
  prettyDerL Hole sigma Nil = "?" <> brackets (sigma MV.!! _g)
  prettyDerL d sigma ss = bug $ "invalid D: " <> show d <> brackets (pretty sigma) <> "(" <> (ss # intercalate ", ") <> ")"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

getTermContext :: forall dr sr. Der dr sr -> Sort sr
getTermContext (DerL _ sigma %% _) = sigma MV.!! _g

--------------------------------------------------------------------------------
-- DR
--------------------------------------------------------------------------------

type DR dr = CursorR (BaseR D dr)

--------------------------------------------------------------------------------

derRules :: forall dr. Ord (Variant (DR dr)) => DerRules (DR dr) SR
derRules = Map.unions
  [ Map.fromFoldable
      [ Free ./\
          ( [] |-
              (Var .% [ g ])
          )
      , Zero ./\
          ( [] |-
              (Var .% [ Ext .% [ g ] ])
          )
      , Suc ./\
          ( [ Var .% [ g ]
            ] |-
              (Var .% [ Ext .% [ g ] ])
          )
      , Ref ./\
          ( [ Var .% [ g ]
            ] |-
              (Term .% [ g ])
          )
      , Lam ./\
          ( [ Term .% [ Ext .% [ g ] ]
            ] |-
              (Term .% [ g ])
          )
      , App ./\
          ( [ Term .% [ g ]
            , Term .% [ g ]
            ] |-
              (Term .% [ g ])
          )
      , Hole ./\
          ( [] |-
              (Term .% [ g ])
          )
      ]
  , Cursor.derRules
  ]

--------------------------------------------------------------------------------

adjRules
  :: forall dr
   . Lacks "bdry" dr
  => Lacks "metaVar" dr
  => Eq (Variant (AdjDerL_DR (DR dr) SR))
  => Ord (Variant (DR dr))
  => AdjDerRules (DR dr) SR
adjRules = modifyAdjRules <> propagationAdjRules
  where
  propagationAdjRules = derive_propagationAdjRules derRules
  modifyAdjRules = List.fromFoldable
    -- these really should be the only necessary rules since we don't have types
    [ makeAdjDerRule
        (Var .% [ Ext .%- [] << dg >> [] ] ↓ Zero .// [ _g /\ g ] % [])
        (Free .// [ _g /\ g' ] % [])
        (\sigma -> _g' `setMetaVar_Sort` ((sigma.changeSort !! _dg) # _outer))
    , makeAdjDerRule
        (Var .% [ Ext .%+ [] << dg >> [] ] ↓ Free .// [ _g /\ g ] % [])
        (Zero .// [ _g /\ g' ] % [])
        (\sigma -> _g' `setMetaVar_Sort` ((sigma.changeSort !! _dg) # _outer))
    ]

--------------------------------------------------------------------------------

editRules
  :: forall dr
   . Lacks "bdry" dr
  => Lacks "metaVar" dr
  => Eq (Variant (AdjDerL_DR (DR dr) SR))
  => Ord (Variant (AdjDerL_DR (DR dr) SR))
  => Show (Variant (AdjDerL_DR (DR dr) SR))
  => Ord (Variant (DR dr))
  => EditRules (DR dr) SR
editRules = List.fromFoldable
  [ {- not using sorted pattern
  EditRule
    { label: "lambda"
    -- , input: d
    , input: d ::% (Term .% [ g ])
    , trans: \sigma -> flip execStateT emptyRecordOfMaps do
        guard $
          derRules
            # getSort_Der (sigma.adjDer !! _d)
            # getLabel
            # V.on (Proxy @"base") (_ == Term) (const false)
        _d `setMetaVar_AdjDer` (sigma.adjDer !! _d)
        _g `setMetaVar_Sort` ((sigma.adjDer !! _d) # getTermContext)
    , output: lam g (Term .% [ Ext .%+ [] << g >> [] ] ↓ d)
    }
  -}
    -- using sorted pattern
    EditRule
      { label: "lambda"
      , input: d ::% (Term .% [ g ])
      , trans: \sigma -> flip execStateT emptyRecordOfMaps do
          _d `setMetaVar_AdjDer` (sigma.adjDer !! _d)
          _g `setMetaVar_Sort` (sigma.sort !! _g)
      , output: lam g (Term .% [ Ext .%+ [] << g >> [] ] ↓ d)
      }
  ]

--------------------------------------------------------------------------------

language
  :: forall dr
   . Lacks "bdry" dr
  => Lacks "metaVar" dr
  => Eq (Variant (AdjDerL_DR (DR dr) SR))
  => Ord (Variant (AdjDerL_DR (DR dr) SR))
  => Show (Variant (AdjDerL_DR (DR dr) SR))
  => Ord (Variant (DR dr))
  => Language (DR dr) SR
language = Language
  { name: "SLC"
  , derRules
  , adjRules
  , editRules
  }

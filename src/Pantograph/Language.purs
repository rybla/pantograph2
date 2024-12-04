module Pantograph.Language where

import Pantograph.Tree
import Prelude

import Control.Alternative (class Alternative)
import Control.Monad.State (StateT, execStateT, get, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.MonadPlus (guard)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, fold, foldM, length, traverse_)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Ord.Generic (genericCompare)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as V
import Pantograph.MetaVar (MetaVar)
import Pantograph.MetaVar as MV
import Pantograph.Pretty (class Pretty, pretty)
import Pantograph.RevList as RevList
import Pantograph.Utility (class IsRecordOfMaps, bug, emptyRecordOfMaps, expand1, todo, uniqueList, unsafeCoerce_because, (##))
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- MetaR
--------------------------------------------------------------------------------

type MetaR r = (metaVar :: MetaVar | r)

makeMetaVarDer :: forall dr sr. MetaVar -> Der (MetaR dr) sr
makeMetaVarDer x = DerL (V.inj (Proxy @"metaVar") x) Map.empty % []

makeMetaVarDer' :: forall dr sr. String -> Der (MetaR dr) sr
makeMetaVarDer' x = DerL (V.inj (Proxy @"metaVar") (MV.MetaVar x)) Map.empty % []

defAndMakeMetaVarDer :: forall dr sr. String -> MetaVar /\ Der (MetaR dr) sr
defAndMakeMetaVarDer str = x /\ makeMetaVarDer x
  where
  x = MV.MetaVar str

makeMetaVarSort :: forall sr. MetaVar -> MetaSort sr
makeMetaVarSort x = V.inj (Proxy @"metaVar") x % []

makeMetaVarSort' :: forall sr. String -> MetaSort sr
makeMetaVarSort' x = V.inj (Proxy @"metaVar") (MV.MetaVar x) % []

defAndMakeMetaVarSort :: forall sr. String -> MetaVar /\ MetaSort sr
defAndMakeMetaVarSort str = x /\ makeMetaVarSort x
  where
  x = MV.MetaVar str

renameMVs :: forall r. (MetaVar -> MetaVar) -> TreeV (MetaR r) -> TreeV (MetaR r)
renameMVs f = map
  ( V.case_
      # (\_ l -> expand1 (Proxy @"metaVar") l)
      # V.on (Proxy @"metaVar") (\x -> V.inj (Proxy @"metaVar") (f x))
  )

collectMVs :: forall r. TreeV (MetaR r) -> List MetaVar
collectMVs =
  map
    ( V.case_
        # mempty
        # V.on (Proxy @"metaVar") pure
    )
    >>> fold
    >>> uniqueList

--------------------------------------------------------------------------------
-- Sort
--------------------------------------------------------------------------------

type Sort sr = Tree (SortL sr)
type SortL sr = Variant (SortR sr)
type SortR (sr :: Row Type) = sr

type MetaSort sr = Tree (MetaSortL sr)
type MetaSortL sr = Variant (MetaSortR sr)
type MetaSortR sr = MetaR sr

type ChangeSort sr = Tree (ChangeSortL sr)
type ChangeSortL sr = Variant (ChangeSortR sr)
type ChangeSortR sr = ChangeR sr

type MetaChangeSort sr = Tree (MetaChangeSortL sr)
type MetaChangeSortL sr = Variant (MetaChangeSortR sr)
type MetaChangeSortR sr = MetaR (ChangeR sr)

makeSort :: forall sr f. Foldable f => SortL sr -> f (Sort sr) -> Sort sr
makeSort sl kids = sl % kids

infix 3 makeSort as ^%

--------------------------------------------------------------------------------
-- BaseR
--------------------------------------------------------------------------------

type BaseR :: Type -> Row Type -> Row Type
type BaseR a r = (base :: a | r)

makeTreeBaseR :: forall f a r. Foldable f => a -> f (TreeV (BaseR a r)) -> TreeV (BaseR a r)
makeTreeBaseR s kids = V.inj (Proxy @"base") s % kids

infix 3 makeTreeBaseR as .%

--------------------------------------------------------------------------------
-- Der
--------------------------------------------------------------------------------

type Der dr sr = Tree (DerL dr sr)
data DerL dr sr = DerL (Variant dr) (SortSubst sr)

type MetaDer dr sr = Tree (MetaDerL dr sr)
type MetaDerL dr sr = DerL (MetaR dr) (MetaR sr)

type PatDer dr sr = Tree (PatDerL dr sr)
type PatDerL dr sr = DerL (PatR (MetaR dr) (MetaR sr)) (MetaR sr)
type PatR dr sr =
  ( sorted :: TreeV sr -- matches against sort of a derivation
  | dr
  )

sorted :: forall dr sr. PatDer dr sr -> MetaSort sr -> PatDer dr sr
sorted d s = V.inj (Proxy @"sorted") s // [] % [ d ]

infix 3 sorted as ::%

type SortSubst sr = MV.Subst (Sort sr)
type MetaSortSubst sr = SortSubst (MetaR sr)

derive instance Generic (DerL dr sr) _

instance (Eq (Variant dr), Eq (Sort sr)) => Eq (DerL dr sr) where
  eq x = genericEq x

instance (Show (Variant dr), Show (Sort sr)) => Show (DerL dr sr) where
  show x = genericShow x

instance (PrettyDerL (Variant dr), Pretty (Sort sr)) => PrettyTreeL (DerL dr sr) where
  prettyTreeL (DerL d sigma) = prettyDerL d (sigma # map pretty)

class PrettyDerL dl where
  prettyDerL :: dl -> MV.Subst String -> List String -> String

instance PrettyDerL MetaVar where
  prettyDerL x sigma Nil | Map.isEmpty sigma = pretty x
  prettyDerL _ _ _ = bug "invalid DerL MetaVar"

instance PrettyDerL_R dr => PrettyDerL (Variant dr) where
  prettyDerL dl sigma kids = prettyDerL_R Proxy sigma kids dl

class PrettyDerL_R dr where
  prettyDerL_R :: Proxy dr -> MV.Subst String -> List String -> Variant dr -> String

instance (RowToList dr drl, PrettyDerL_RL dr drl) => PrettyDerL_R dr where
  prettyDerL_R = prettyDerL_RL (Proxy @drl)

class PrettyDerL_RL dr (drl :: RowList Type) | drl -> dr where
  prettyDerL_RL :: Proxy drl -> Proxy dr -> MV.Subst String -> List String -> Variant dr -> String

instance PrettyDerL_RL () RowList.Nil where
  prettyDerL_RL _ _ _ _ = V.case_

instance
  ( IsSymbol x
  , PrettyDerL a
  , Cons x a r r'
  , PrettyDerL_RL r rl
  ) =>
  PrettyDerL_RL r' (RowList.Cons x a rl) where
  prettyDerL_RL _ _ sigma kids =
    prettyDerL_RL (Proxy @rl) (Proxy @r) sigma kids
      # V.on (Proxy @x) (\a -> prettyDerL a sigma kids)

makeDerL dl sigma = DerL dl (Map.fromFoldable sigma)

infix 4 makeDerL as //

--------------------------------------------------------------------------------
-- BaseDerR 
--------------------------------------------------------------------------------

makeBaseDerL d sigma = DerL (V.inj (Proxy @"base") d) (Map.fromFoldable sigma)

infix 4 makeBaseDerL as .//

--------------------------------------------------------------------------------
-- DerRule
--------------------------------------------------------------------------------

type DerRules dr sr = Map (Variant dr) (DerRule sr)

data DerRule sr = DerRule
  { sort :: Sort (MetaR sr)
  , kids :: List (Sort (MetaR sr))
  }

makeTupleBaseDerAndDerRule d rule = V.inj (Proxy @"base") d /\ rule

infix 0 makeTupleBaseDerAndDerRule as ./\

makeDerRule :: forall sr f. Foldable f => Sort (MetaR sr) -> f (Sort (MetaR sr)) -> DerRule sr
makeDerRule sort kids = DerRule
  { sort
  , kids: kids # List.fromFoldable
  }

infix 1 makeDerRule as -|

makeDerRuleFlipped = flip makeDerRule

infix 1 makeDerRuleFlipped as |-

getDerRule :: forall dr sr. Show (Variant dr) => Ord (Variant dr) => Variant dr -> DerRules dr sr -> DerRule sr
getDerRule d derRules = derRules # Map.lookup d # fromMaybe' \_ -> bug $ "unknown derivation label: " <> show d

applyMetaVarSubst_TreeV :: forall r. MV.Subst (TreeV r) -> TreeV (MetaR r) -> TreeV r
applyMetaVarSubst_TreeV sigma (l %% kids) =
  l ## V.case_
    # (\_ l' -> l' %% (kids <#> applyMetaVarSubst_TreeV sigma))
    # V.on (Proxy @"metaVar") (sigma MV.!! _)

getSort_DerL :: forall dr sr. Show (Variant dr) => Ord (Variant dr) => DerRules dr sr -> DerL dr sr -> Sort sr
getSort_DerL derRules (DerL dr sigma) = applyMetaVarSubst_TreeV sigma sort
  where
  DerRule { sort } = derRules # getDerRule dr

getSort_Der :: forall dr sr. Show (Variant dr) => Ord (Variant dr) => Map (Variant dr) (DerRule sr) -> Tree (DerL dr sr) -> Sort sr
getSort_Der derRules (derL %% _) = getSort_DerL derRules derL

getSort_AdjDerL :: forall dr sr. Show (Variant dr) => Ord (Variant dr) => Eq (Variant sr) => AdjDerL dr sr -> DerRules dr sr -> Sort sr
getSort_AdjDerL (DerL dr sigma) derRules =
  dr #
    V.on (Proxy @"bdry") (\(Bdry _ ch) -> ch # _outer)
      (\dr' -> getSort_DerL derRules (DerL dr' sigma))

getSort_AdjDer :: forall dr sr. Show (Variant dr) => Ord (Variant dr) => Eq (Variant sr) => Tree (AdjDerL dr sr) -> Map (Variant dr) (DerRule sr) -> Sort sr
getSort_AdjDer (adjL %% _) = getSort_AdjDerL adjL

--------------------------------------------------------------------------------
-- AdjDer
--------------------------------------------------------------------------------

type AdjDer dr sr = Tree (AdjDerL dr sr)
type AdjDerL dr sr = DerL (AdjDerL_DR dr sr) sr
type AdjDerL_DR dr sr = (bdry :: Bdry sr | dr)

type MetaAdjDer dr sr = Tree (MetaAdjDerL dr sr)
type MetaAdjDerL dr sr = AdjDerL (MetaR dr) (MetaR sr)

data Bdry (sr :: Row Type) = Bdry BdryDir (ChangeSort sr)

type MetaBdry sr = Bdry (MetaR sr)

derive instance Generic (Bdry sr) _

instance Eq (ChangeSort sr) => Eq (Bdry sr) where
  eq x = genericEq x

instance (Eq (ChangeSort sr), Ord (ChangeSort sr)) => Ord (Bdry sr) where
  compare x = genericCompare x

instance Show (ChangeSort sr) => Show (Bdry sr) where
  show x = genericShow x

instance Pretty (ChangeSort sr) => Pretty (Bdry sr) where
  pretty (Bdry dir ch) = "{{ " <> pretty ch <> " " <> pretty dir <> " _ }}"

instance Pretty (ChangeSort sr) => PrettyDerL (Bdry sr) where
  prettyDerL (Bdry dir ch) sigma (kid : Nil) | Map.isEmpty sigma = "{{ " <> pretty ch <> " " <> pretty dir <> " " <> kid <> " }}"
  prettyDerL _ _ _ = bug "invalid DerL Bdry"

data BdryDir = Up | Down

derive instance Generic BdryDir _

derive instance Eq BdryDir

derive instance Ord BdryDir

instance Show BdryDir where
  show x = genericShow x

instance Pretty BdryDir where
  pretty Up = "↑"
  pretty Down = "↓"

applyFunction :: forall a b. (a -> b) -> a -> b
applyFunction f a = f a

infixl 2 applyFunction as <<
infixl 2 applyFunction as >>

makeSortChangePlus :: forall sr f_l f_r. Foldable f_l => Foldable f_r => SortL sr -> f_l (Sort sr) -> ChangeSort sr -> f_r (Sort sr) -> ChangeSort sr
makeSortChangePlus l ls kid rs =
  V.inj (Proxy @"plus") (PlusChange $ Tooth l (RevList.fromFoldable ls) (List.fromFoldable rs)) %
    [ kid ]

infixl 2 makeSortChangePlus as %+

makeSortChangeMinus :: forall sr f_l f_r. Foldable f_l => Foldable f_r => SortL sr -> f_l (Sort sr) -> ChangeSort sr -> f_r (Sort sr) -> ChangeSort sr
makeSortChangeMinus l ls kid rs =
  V.inj (Proxy @"minus") (MinusChange $ Tooth l (RevList.fromFoldable ls) (List.fromFoldable rs)) %
    [ kid ]

infixl 2 makeSortChangeMinus as %-

makeTreeBaseRChangePlus :: forall s sr f_l f_r. Foldable f_l => Foldable f_r => s -> f_l (Sort (BaseR s sr)) -> ChangeSort (BaseR s sr) -> f_r (Sort (BaseR s sr)) -> ChangeSort (BaseR s sr)
makeTreeBaseRChangePlus l ls kid rs =
  V.inj (Proxy @"plus") (PlusChange $ Tooth (V.inj (Proxy @"base") l) (RevList.fromFoldable ls) (List.fromFoldable rs)) %
    [ kid ]

infixl 2 makeTreeBaseRChangePlus as .%+

makeTreeBaseRChangeMinus :: forall s sr f_l f_r. Foldable f_l => Foldable f_r => s -> f_l (Sort (BaseR s sr)) -> ChangeSort (BaseR s sr) -> f_r (Sort (BaseR s sr)) -> ChangeSort (BaseR s sr)
makeTreeBaseRChangeMinus l ls kid rs =
  V.inj (Proxy @"minus") (MinusChange $ Tooth (V.inj (Proxy @"base") l) (RevList.fromFoldable ls) (List.fromFoldable rs)) %
    [ kid ]

infixl 2 makeTreeBaseRChangeMinus as .%-

makeAdjDerBdryDown :: forall dr sr. ChangeSort sr -> AdjDer dr sr -> AdjDer dr sr
makeAdjDerBdryDown ch kid =
  DerL (V.inj (Proxy @"bdry") (Bdry Down ch)) Map.empty %
    [ kid ]

makeAdjDerBdryUp :: forall dr sr. ChangeSort sr -> AdjDer dr sr -> AdjDer dr sr
makeAdjDerBdryUp ch kid =
  DerL (V.inj (Proxy @"bdry") (Bdry Up ch)) Map.empty %
    [ kid ]

infix 2 makeAdjDerBdryDown as ↓
infix 2 makeAdjDerBdryUp as ↑

type AdjDerSubst dr sr =
  { adjDer :: MV.Subst (AdjDer dr sr)
  , changeSort :: MV.Subst (ChangeSort sr)
  , sort :: MV.Subst (Sort sr)
  }

applyAdjDerSubst_Sort :: forall dr sr. AdjDerSubst dr sr -> MetaSort sr -> Sort sr
applyAdjDerSubst_Sort (sigma@{ sort }) (l %% kids) =
  l ## V.case_
    # (\_ l' -> l' %% (kids # map (applyAdjDerSubst_Sort sigma)))
    # V.on (Proxy @"metaVar") (\x -> sort MV.!! x)

applyAdjDerSubst_ChangeSort :: forall dr sr. AdjDerSubst dr sr -> MetaChangeSort sr -> ChangeSort sr
applyAdjDerSubst_ChangeSort (sigma@{ changeSort }) (l %% kids) =
  l ## V.case_
    # (\_ l' -> l' %% (kids # map (applyAdjDerSubst_ChangeSort sigma)))
    # V.on (Proxy @"metaVar") (\x -> changeSort MV.!! x)

applyAdjDerSubst_AdjDer :: forall dr sr. AdjDerSubst dr sr -> MetaAdjDer dr sr -> AdjDer dr sr
applyAdjDerSubst_AdjDer (sigma@{ adjDer }) (DerL l sigma_d %% kids) =
  l ## V.case_
    #
      ( \_ l' -> DerL (unsafeCoerce_because "constant expansion of sr" l') (sigma_d <#> applyAdjDerSubst_Sort sigma) %%
          (kids # map (applyAdjDerSubst_AdjDer sigma))
      )
    # V.on (Proxy @"bdry")
        ( \(Bdry dir ch) ->
            DerL (V.inj (Proxy @"bdry") (Bdry dir (ch # applyAdjDerSubst_ChangeSort sigma))) (sigma_d <#> applyAdjDerSubst_Sort sigma) %%
              (kids # map (applyAdjDerSubst_AdjDer sigma))
        )
    # V.on (Proxy @"metaVar") (\x -> adjDer MV.!! x)

--------------------------------------------------------------------------------
-- AdjDerSubstTrans
--------------------------------------------------------------------------------

type AdjDerSubstTrans dr sr = AdjDerSubst dr sr -> Maybe (AdjDerSubst dr sr)

-- TODO: deep embedding of AdjDerSubstTrans so that can informatively show/render
-- data AdjDerSubstTrans dr sr
--   = Shallow_AdjDerSubstTrans (AdjDerSubst dr sr -> Maybe (AdjDerSubst dr sr))
--   | Deep_AdjDerSubstTrans (forall a. MV.Subst a -> MV.Subst (TransExpr a))

-- data TransExpr a
--   = Inject_TransExpr a
--   | Op_TransExpr TransExprOp (Array (TransExpr a))

-- data TransExprOp = InnerEndpoint_TransExprOp

-- instance (Show (MetaAdjDer dr sr)) => Show (AdjDerSubstTrans dr sr) where
--   show (Shallow_AdjDerSubstTrans _) = "<function>"
--   show (Deep_AdjDerSubstTrans _) = todo "TODO: (Show (AdjDerSubstTrans dr sr)).show"

--------------------------------------------------------------------------------
-- AdjDerRules
--------------------------------------------------------------------------------

type AdjDerRules dr sr = List (AdjDerRule dr sr)

data AdjDerRule dr sr = AdjDerRule
  { input :: MetaAdjDer dr sr
  , trans :: AdjDerSubstTrans dr sr
  , output :: MetaAdjDer dr sr
  }

instance (Show (MetaAdjDer dr sr)) => Show (AdjDerRule dr sr) where
  show (AdjDerRule { input, trans: _, output }) =
    "AdjDerRule { input: " <> show input <> ", output: " <> show output <> ", trans: <function>" <> " }"

instance (Pretty (MetaAdjDer dr sr)) => Pretty (AdjDerRule dr sr) where
  pretty (AdjDerRule { input, trans: _, output }) =
    pretty input <> "  ~~>  " <> pretty output <> "  with  <function>"

makeAdjDerRule
  :: forall dr sr
   . MetaAdjDer dr sr
  -> MetaAdjDer dr sr
  -> ( Record (SubstAdjDer dr sr (SubstChangeSort sr (SubstSort sr ())))
       -> StateT (Record (SubstAdjDer dr sr (SubstChangeSort sr (SubstSort sr ())))) Maybe Unit
     )
  -> AdjDerRule dr sr
makeAdjDerRule input output trans = AdjDerRule
  { input
  , output
  , trans: \sigma -> flip execStateT emptyRecordOfMaps $ trans sigma
  }

applyAdjDerRule
  :: forall dr sr
   . Eq (Variant sr)
  => Eq (Variant (ChangeR sr))
  => Eq (Variant dr)
  => Eq (AdjDerL dr sr)
  => AdjDerRule dr sr
  -> AdjDer dr sr
  -> Maybe (AdjDer dr sr)
applyAdjDerRule (AdjDerRule { input, output, trans }) adj = do
  _ /\ sigma_input <- adj # matchAdjDer input # runMatchM
  sigma_output <- trans sigma_input
  let output' = applyAdjDerSubst_AdjDer sigma_output output
  -- Debug.logM 0 $ intercalate "\n"
  --   [ "applyAdjDerRule.try match"
  --   , "  - input        = " <> pretty input
  --   , "  - adj          = " <> pretty adj
  --   , "  - output       = " <> pretty output
  --   , "  - sigma_output = " <> indent 2 (pretty sigma_output)
  --   , "  - output' = " <> pretty output'
  --   ]
  pure output'

--------------------------------------------------------------------------------
-- EditRules
--------------------------------------------------------------------------------

type EditRules dr sr = List (EditRule dr sr)

data EditRule dr sr = EditRule
  { label :: String
  -- , input :: MetaDer dr sr
  , input :: PatDer dr sr
  , trans :: AdjDerSubstTrans dr sr
  , output :: MetaAdjDer dr sr
  }

-- TODO
-- applyEditRule :: forall dr sr. EditRule dr sr -> Der dr sr -> Maybe (AdjDer dr sr)
-- applyEditRule (EditRule rule) dt = do
--   sigma <- matchDer rule.input dt # runMatchAdjDerSubstM
--   sigma' <- rule.trans sigma
--   pure $ applyAdjDerSubst_AdjDer sigma' rule.output

--------------------------------------------------------------------------------
-- Language
--------------------------------------------------------------------------------

data Language dr sr = Language
  { name :: String
  , derRules :: DerRules dr sr
  , adjRules :: AdjDerRules dr sr
  , editRules :: EditRules dr sr
  }

--------------------------------------------------------------------------------
-- MatchDerSubstM
--------------------------------------------------------------------------------

type MatchM r = StateT (Record r) Maybe

runMatchM :: forall r a. IsRecordOfMaps r => MatchM r a -> Maybe (a /\ Record r)
runMatchM ma = runStateT ma emptyRecordOfMaps

type SubstDer dr sr r = (der :: MV.Subst (Der dr sr) | r)
type SubstAdjDer dr sr r = (adjDer :: MV.Subst (AdjDer dr sr) | r)
type SubstChangeSort sr r = (changeSort :: MV.Subst (ChangeSort sr) | r)
type SubstSort sr r = (sort :: MV.Subst (Sort sr) | r)

--------------------------------------------------------------------------------
-- MatchDerSubstM
--------------------------------------------------------------------------------

matchDerL :: forall dr sr r. Eq (Variant dr) => Eq (Variant sr) => DerL dr (MetaR sr) -> DerL dr sr -> MatchM (SubstSort sr r) Unit
matchDerL (DerL d1 sigma1) (DerL d2 sigma2) = do
  guard $ d1 == d2
  matchSortSubst sigma1 sigma2

matchDer :: forall dr sr r. Eq (DerL dr sr) => Eq (Variant dr) => Eq (Variant (MetaR dr)) => Eq (Variant sr) => MetaDer dr sr -> Der dr sr -> MatchM (SubstDer dr sr (SubstSort sr r)) Unit
matchDer (DerL dll1 sigma1 %% kids1) d2@(dl2 %% kids2) = do
  dll1 ## V.case_
    #
      ( \_ dll1' -> do
          matchDerL (DerL dll1' sigma1) dl2
          List.zip kids1 kids2 # traverse_ (uncurry matchDer)
      )
    # V.on (Proxy @"metaVar") (\x -> setMetaVar_Der x d2)

matchDer_Pat
  :: forall dr sr r
   . Show (Variant dr)
  => Eq (Variant dr)
  => Ord (Variant dr)
  => Eq (Variant sr)
  => Eq (DerL dr sr)
  => DerRules dr sr
  -> PatDer dr sr
  -> Der dr sr
  -> MatchM (SubstDer dr sr (SubstSort sr r)) Unit
matchDer_Pat derRules (DerL dll1 sigma1 %% kids1) d2@(dl2 %% kids2) = do
  dll1 ## V.case_
    #
      ( \_ dll1' -> do
          matchDerL (DerL dll1' sigma1) dl2
          if (kids1 # length :: Int) /= (kids2 # length) then bug "PatDer and Der of same label have different numbers of kids" else pure unit
          List.zip kids1 kids2 # traverse_ (uncurry (matchDer_Pat derRules))
      )
    # V.on (Proxy @"sorted")
        ( \sort_pat -> do
            if (kids1 # length) /= 1 then bug "sorted label of PatDer has non-1 kids" else pure unit
            matchSort sort_pat (d2 # getSort_Der derRules)
            List.zip kids1 kids2 # traverse_ (uncurry (matchDer_Pat derRules))
        )
    # V.on (Proxy @"metaVar")
        ( \x -> do
            if (kids1 # length) /= 0 then bug "metavar label of PatDer has non-0 kids" else pure unit
            setMetaVar_Der x d2
        )

setMetaVar_Sort :: forall sr r. Eq (Sort sr) => MetaVar -> Sort sr -> MatchM (SubstSort sr r) Unit
setMetaVar_Sort x sort = do
  sigma <- get
  case sigma.sort # Map.lookup x of
    Nothing -> put sigma { sort = sigma.sort # Map.insert x sort }
    Just sort' | sort == sort' -> pure unit
    Just _ | otherwise -> pure unit

setMetaVar_ChangeSort :: forall sr r. Eq (ChangeSort sr) => MetaVar -> ChangeSort sr -> MatchM (SubstChangeSort sr r) Unit
setMetaVar_ChangeSort x ch = do
  sigma <- get
  case sigma.changeSort # Map.lookup x of
    Nothing -> put sigma { changeSort = sigma.changeSort # Map.insert x ch }
    Just ch' | ch == ch' -> pure unit
    Just _ | otherwise -> pure unit

setMetaVar_Der :: forall dr sr r. Eq (Der dr sr) => MetaVar -> Der dr sr -> MatchM (SubstDer dr sr r) Unit
setMetaVar_Der x adj = do
  sigma <- get
  case sigma.der # Map.lookup x of
    Nothing -> put sigma { der = sigma.der # Map.insert x adj }
    Just adj' | adj == adj' -> pure unit
    Just _ | otherwise -> pure unit

setMetaVar_AdjDer :: forall dr sr r. Eq (AdjDer dr sr) => MetaVar -> AdjDer dr sr -> MatchM (SubstAdjDer dr sr r) Unit
setMetaVar_AdjDer x adj = do
  sigma <- get
  case sigma.adjDer # Map.lookup x of
    Nothing -> put sigma { adjDer = sigma.adjDer # Map.insert x adj }
    Just adj' | adj == adj' -> pure unit
    Just _ | otherwise -> pure unit

matchSort :: forall sr r. Eq (Variant sr) => MetaSort sr -> Sort sr -> MatchM (SubstSort sr r) Unit
matchSort (sl1 %% kids1) sort2@(sl2 %% kids2) =
  sl1 ## V.case_
    #
      ( \_ sl1' -> do
          guard $ sl1' == sl2
          List.zip kids1 kids2 # traverse_ (uncurry matchSort)
      )
    # V.on (Proxy @"metaVar") (\x -> setMetaVar_Sort x sort2)

matchSortSubst :: forall sr r. Eq (Sort sr) => Eq (Variant sr) => MetaSortSubst sr -> SortSubst sr -> MatchM (SubstSort sr r) Unit
matchSortSubst ss1 ss2 =
  Map.keys ss1 `Set.union` Map.keys ss2 # traverse_ \x -> do
    ms1 <- ss1 # Map.lookup x # lift
    s2 <- ss2 # Map.lookup x # lift
    matchSort ms1 s2

matchChangeSort :: forall sr r. Eq (ChangeSort sr) => Eq (Variant (ChangeSortR sr)) => MetaChangeSort sr -> ChangeSort sr -> MatchM (SubstChangeSort sr r) Unit
matchChangeSort (mch1 %% kids1) ch2@(chl2 %% kids2) =
  mch1 ## V.case_
    #
      ( \_ chl1 -> do
          guard $ chl1 == chl2
          List.zip kids1 kids2 # traverse_ (uncurry matchChangeSort)
      )
    # V.on (Proxy @"metaVar") (\x -> setMetaVar_ChangeSort x ch2)

matchBdry :: forall sr r. Eq (Variant (ChangeSortR sr)) => MetaBdry sr -> Bdry sr -> MatchM (SubstChangeSort sr r) Unit
matchBdry (Bdry dir1 ch1) (Bdry dir2 ch2) = do
  guard $ dir1 == dir2
  matchChangeSort ch1 ch2

matchAdjDerL
  :: forall dr sr r
   . Eq (Variant sr)
  => Eq (Variant (ChangeR sr))
  => Eq (Variant dr)
  => AdjDerL dr (MetaR sr)
  -> AdjDerL dr sr
  -> MatchM (SubstAdjDer dr sr (SubstChangeSort sr (SubstSort sr r))) Unit
matchAdjDerL (DerL l1 sigma1) (DerL l2 sigma2) = do
  l1 ## V.case_
    #
      ( \_ l1' -> l2 ## V.case_
          # (\_ l2' -> guard $ (l1' :: Variant dr) == l2')
          # V.on (Proxy @"bdry") (\_bdry2 -> pure unit)
      )
    # V.on (Proxy @"bdry")
        ( \bdry1 -> l2 ## V.case_
            # (\_ _ -> pure unit)
            # V.on (Proxy @"bdry") (\bdry2 -> matchBdry bdry1 bdry2)
        )
  matchSortSubst sigma1 sigma2

matchAdjDer
  :: forall dr sr r
   . Eq (Variant sr)
  => Eq (Variant (ChangeR sr))
  => Eq (Variant dr)
  => Eq (AdjDerL dr sr)
  => MetaAdjDer dr sr
  -> AdjDer dr sr
  -> MatchM (SubstAdjDer dr sr (SubstChangeSort sr (SubstSort sr r))) Unit
matchAdjDer (DerL l1 sigma1 %% kids1) a2@(l2 %% kids2) =
  l1 ## V.case_
    #
      ( \_ l1' -> do
          matchAdjDerL (DerL l1' sigma1) l2
          List.zip kids1 kids2 # traverse_ (uncurry matchAdjDer)
      )
    # V.on (Proxy @"metaVar") (\x -> setMetaVar_AdjDer x a2)

union_AdjDerSubst
  :: forall dr sr
   . Eq (AdjDerL dr sr)
  => Eq (Variant sr)
  => Eq (Variant (ChangeR sr))
  => AdjDerSubst dr sr
  -> AdjDerSubst dr sr
  -> Maybe (AdjDerSubst dr sr)
union_AdjDerSubst sigma1 sigma2 = do
  adjDer <- sigma1.adjDer # Map.toUnfoldable # List.foldM insertIfEq sigma2.adjDer
  changeSort <- sigma1.changeSort # Map.toUnfoldable # List.foldM insertIfEq sigma2.changeSort
  sort <- sigma1.sort # Map.toUnfoldable # List.foldM insertIfEq sigma2.sort
  pure { adjDer, changeSort, sort }

unions_AdjDerSubst
  :: forall dr sr f
   . Eq (AdjDerL dr sr)
  => Eq (Variant sr)
  => Eq (Variant (ChangeR sr))
  => Foldable f
  => f (AdjDerSubst dr sr)
  -> Maybe (AdjDerSubst dr sr)
unions_AdjDerSubst = foldM union_AdjDerSubst emptyRecordOfMaps

insertIfEq :: forall m k v. Monad m => Alternative m => Ord k => Eq v => Map k v -> k /\ v -> m (Map k v)
insertIfEq sigma (x /\ a) = case sigma # Map.lookup x of
  Nothing -> pure $ sigma # Map.insert x a
  Just a' -> do
    guard $ a == a'
    pure sigma

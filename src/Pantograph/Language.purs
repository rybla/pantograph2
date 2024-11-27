module Pantograph.Language where

import Pantograph.Tree
import Prelude

import Control.Alternative (class Alternative)
import Control.Monad.State (StateT, execStateT, get, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.MonadPlus (guard)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, fold, foldM, traverse_)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
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

_metaVar = Proxy :: Proxy "metaVar"

makeMetaVarDer :: forall dr sr. MetaVar -> Der (MetaR dr) sr
makeMetaVarDer x = DerL (V.inj _metaVar x) Map.empty % []

makeMetaVarDer' :: forall dr sr. String -> Der (MetaR dr) sr
makeMetaVarDer' x = DerL (V.inj _metaVar (MV.MetaVar x)) Map.empty % []

defAndMakeMetaVarDer :: forall dr sr. String -> MetaVar /\ Der (MetaR dr) sr
defAndMakeMetaVarDer str = x /\ makeMetaVarDer x
  where
  x = MV.MetaVar str

makeMetaVarSort :: forall sr. MetaVar -> MetaSort sr
makeMetaVarSort x = V.inj _metaVar x % []

makeMetaVarSort' :: forall sr. String -> MetaSort sr
makeMetaVarSort' x = V.inj _metaVar (MV.MetaVar x) % []

defAndMakeMetaVarSort :: forall sr. String -> MetaVar /\ MetaSort sr
defAndMakeMetaVarSort str = x /\ makeMetaVarSort x
  where
  x = MV.MetaVar str

renameMVs :: forall r. (MetaVar -> MetaVar) -> TreeV (MetaR r) -> TreeV (MetaR r)
renameMVs f = map
  ( V.case_
      # (\_ l -> expand1 (Proxy :: Proxy "metaVar") l)
      # V.on _metaVar (\x -> V.inj _metaVar (f x))
  )

collectMVs :: forall r. TreeV (MetaR r) -> List MetaVar
collectMVs =
  map
    ( V.case_
        # mempty
        # V.on _metaVar pure
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

--------------------------------------------------------------------------------
-- BaseSortR
--------------------------------------------------------------------------------

type BaseSortR :: Type -> Row Type -> Row Type
type BaseSortR s r = (baseSort :: s | r)

_baseSort = Proxy :: Proxy "baseSort"

makeBaseSort :: forall s sr f. Foldable f => s -> f (Sort (BaseSortR s sr)) -> Sort (BaseSortR s sr)
makeBaseSort s kids = V.inj _baseSort s % kids

infix 3 makeBaseSort as ^%

--------------------------------------------------------------------------------
-- Der
--------------------------------------------------------------------------------

type Der dr sr = Tree (DerL dr sr)
data DerL dr sr = DerL (Variant dr) (SortSubst sr)

type MetaDer dr sr = Tree (MetaDerL dr sr)
type MetaDerL dr sr = DerL (MetaR dr) (MetaR sr)

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

instance PrettyDerL_R dr => PrettyDerL (Variant dr) where
  prettyDerL dl sigma kids = prettyDerL_R Proxy sigma kids dl

class PrettyDerL_R dr where
  prettyDerL_R :: Proxy dr -> MV.Subst String -> List String -> Variant dr -> String

instance (RowToList dr drl, PrettyDerL_RL dr drl) => PrettyDerL_R dr where
  prettyDerL_R = prettyDerL_RL (Proxy :: Proxy drl)

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
    prettyDerL_RL (Proxy :: Proxy rl) (Proxy :: Proxy r) sigma kids
      # V.on (Proxy :: Proxy x) (\a -> prettyDerL a sigma kids)

--------------------------------------------------------------------------------
-- BaseDerR 
--------------------------------------------------------------------------------

type BaseDerR :: Type -> Row Type -> Row Type
type BaseDerR d dr = (baseDer :: d | dr)

_baseDer = Proxy :: Proxy "baseDer"

makeBaseDerL :: forall d dr sr. d -> SortSubst sr -> DerL (BaseDerR d dr) sr
makeBaseDerL d sigma = DerL (V.inj _baseDer d) sigma

infix 3 makeBaseDerL as //

--------------------------------------------------------------------------------
-- DerRule
--------------------------------------------------------------------------------

type DerRules dr sr = Map (Variant dr) (DerRule sr)

data DerRule sr = DerRule
  { sort :: Sort (MetaR sr)
  , kids :: List (Sort (MetaR sr))
  }

_sort = Proxy :: Proxy "sort"
_kids = Proxy :: Proxy "kids"

makeDerRule :: forall sr f. Foldable f => Sort (MetaR sr) -> f (Sort (MetaR sr)) -> DerRule sr
makeDerRule sort kids = DerRule
  { sort
  , kids: kids # List.fromFoldable
  }

infix 1 makeDerRule as -|

makeDerRuleFlipped = flip makeDerRule

infix 1 makeDerRuleFlipped as |-

getDerRule :: forall dr sr. Pretty (Variant dr) => Ord (Variant dr) => Variant dr -> DerRules dr sr -> DerRule sr
getDerRule d derRules = derRules # Map.lookup d # fromMaybe' \_ -> bug $ "unknown derivation label: " <> pretty d

applyMetaVarSubst_TreeV :: forall r. MV.Subst (TreeV r) -> TreeV (MetaR r) -> TreeV r
applyMetaVarSubst_TreeV sigma (l %% kids) =
  l ## V.case_
    # (\_ l' -> l' %% (kids <#> applyMetaVarSubst_TreeV sigma))
    # V.on _metaVar (sigma MV.!! _)

getSort_DerL :: forall dr sr. Pretty (Variant dr) => Ord (Variant dr) => DerL dr sr -> DerRules dr sr -> Sort sr
getSort_DerL (DerL dr sigma) derRules = applyMetaVarSubst_TreeV sigma sort
  where
  DerRule { sort } = derRules # getDerRule dr

getSort_Der :: forall dr sr. Pretty (Variant dr) => Ord (Variant dr) => Tree (DerL dr sr) -> Map (Variant dr) (DerRule sr) -> Tree (Variant sr)
getSort_Der (derL %% _) = getSort_DerL derL

getSort_AdjDerL :: forall dr sr. Pretty (Variant dr) => Ord (Variant dr) => Eq (Variant sr) => AdjDerL dr sr -> DerRules dr sr -> Sort sr
getSort_AdjDerL (DerL dr sigma) derRules =
  dr #
    V.on _bdry (\(Bdry _ ch) -> ch # outerEndpoint)
      (\dr' -> getSort_DerL (DerL dr' sigma) derRules)

getSort_AdjDer :: forall dr sr. Pretty (Variant dr) => Ord (Variant dr) => Eq (Variant sr) => Tree (AdjDerL dr sr) -> Map (Variant dr) (DerRule sr) -> Tree (Variant sr)
getSort_AdjDer (adjL %% _) = getSort_AdjDerL adjL

--------------------------------------------------------------------------------
-- AdjDer
--------------------------------------------------------------------------------

type AdjDer dr sr = Tree (AdjDerL dr sr)
type AdjDerL dr sr = DerL (bdry :: Bdry sr | dr) sr

type MetaAdjDer dr sr = Tree (MetaAdjDerL dr sr)
type MetaAdjDerL dr sr = AdjDerL (MetaR dr) (MetaR sr)

_bdry = Proxy :: Proxy "bdry"

data Bdry (sr :: Row Type) = Bdry BdryDir (ChangeSort sr)

type MetaBdry sr = Bdry (MetaR sr)

derive instance Generic (Bdry sr) _

instance Eq (ChangeSort sr) => Eq (Bdry sr) where
  eq x = genericEq x

instance Show (ChangeSort sr) => Show (Bdry sr) where
  show x = genericShow x

instance Pretty (ChangeSort sr) => Pretty (Bdry sr) where
  pretty (Bdry dir ch) = "{{ " <> pretty ch <> " " <> pretty dir <> " _ }}"

instance Pretty (ChangeSort sr) => PrettyTreeL (Bdry sr) where
  prettyTreeL (Bdry dir ch) (kid : Nil) = "{{ " <> pretty ch <> " " <> pretty dir <> " " <> kid <> " }}"
  prettyTreeL _ _ = bug "invalid Bdry"

data BdryDir = Up | Down

derive instance Generic BdryDir _

derive instance Eq BdryDir

instance Show BdryDir where
  show x = genericShow x

instance Pretty BdryDir where
  pretty Up = "↑"
  pretty Down = "↓"

applyFunction :: forall a b. (a -> b) -> a -> b
applyFunction f a = f a

infixl 2 applyFunction as <<
infixl 2 applyFunction as >>

makeAdjDerBdryDownPlus :: forall sr dr f_l f_r. Foldable f_l => Foldable f_r => SortL sr -> f_l (Sort sr) -> AdjDer dr sr -> f_r (Sort sr) -> AdjDer dr sr
makeAdjDerBdryDownPlus l ls kid rs =
  DerL (V.inj _bdry $ Bdry Down $ (V.inj _plus (PlusChange $ Tooth l (RevList.fromFoldable ls) (List.fromFoldable rs))) % [ todo "kid_sort" ]) Map.empty %
    [ kid ]

makeAdjDerBdryDownMinus :: forall sr dr f_l f_r. Foldable f_l => Foldable f_r => SortL sr -> f_l (Sort sr) -> AdjDer dr sr -> f_r (Sort sr) -> AdjDer dr sr
makeAdjDerBdryDownMinus l ls kid rs =
  DerL (V.inj _bdry $ Bdry Down $ (V.inj _minus (MinusChange $ Tooth l (RevList.fromFoldable ls) (List.fromFoldable rs))) % [ todo "kid_sort" ]) Map.empty %
    [ kid ]

infixl 2 makeAdjDerBdryDownPlus as %+
infixl 2 makeAdjDerBdryDownMinus as %-

makeAdjDerBdryDown :: forall dr sr. ChangeSort sr -> AdjDer dr sr -> AdjDer dr sr
makeAdjDerBdryDown ch kid =
  DerL (V.inj _bdry (Bdry Down ch)) Map.empty %
    [ kid ]

makeAdjDerBdryUp :: forall dr sr. ChangeSort sr -> AdjDer dr sr -> AdjDer dr sr
makeAdjDerBdryUp ch kid =
  DerL (V.inj _bdry (Bdry Up ch)) Map.empty %
    [ kid ]

infix 2 makeAdjDerBdryDown as ↓
infix 2 makeAdjDerBdryUp as ↑

type AdjDerSubst dr sr =
  { adjDer :: MV.Subst (AdjDer dr sr)
  , changeSort :: MV.Subst (ChangeSort sr)
  , sort :: MV.Subst (Sort sr)
  }

_adjs = Proxy :: Proxy "adjs"
_chs = Proxy :: Proxy "chs"
_sorts = Proxy :: Proxy "sorts"

applyAdjDerSubst_Sort :: forall dr sr. AdjDerSubst dr sr -> MetaSort sr -> Sort sr
applyAdjDerSubst_Sort (sigma@{ sort }) (l %% kids) =
  l ## V.case_
    # (\_ l' -> l' %% (kids # map (applyAdjDerSubst_Sort sigma)))
    # V.on _metaVar (\x -> sort MV.!! x)

applyAdjDerSubst_ChangeSort :: forall dr sr. AdjDerSubst dr sr -> MetaChangeSort sr -> ChangeSort sr
applyAdjDerSubst_ChangeSort (sigma@{ changeSort }) (l %% kids) =
  l ## V.case_
    # (\_ l' -> l' %% (kids # map (applyAdjDerSubst_ChangeSort sigma)))
    # V.on _metaVar (\x -> changeSort MV.!! x)

applyAdjDerSubst_AdjDer :: forall dr sr. AdjDerSubst dr sr -> MetaAdjDer dr sr -> AdjDer dr sr
applyAdjDerSubst_AdjDer (sigma@{ adjDer }) (DerL l sigma_d %% kids) =
  l ## V.case_
    #
      ( \_ l' -> DerL (unsafeCoerce_because "constant expansion of sr" l') (sigma_d <#> applyAdjDerSubst_Sort sigma) %%
          (kids # map (applyAdjDerSubst_AdjDer sigma))
      )
    # V.on _bdry
        ( \(Bdry dir ch) ->
            DerL (V.inj _bdry (Bdry dir (ch # applyAdjDerSubst_ChangeSort sigma))) (sigma_d <#> applyAdjDerSubst_Sort sigma) %%
              (kids # map (applyAdjDerSubst_AdjDer sigma))
        )
    # V.on _metaVar (\x -> adjDer MV.!! x)

--------------------------------------------------------------------------------
-- AdjDerRules
--------------------------------------------------------------------------------

type AdjDerRules dr sr = List (AdjDerRule dr sr)

data AdjDerRule dr sr = AdjDerRule
  { input :: MetaAdjDer dr sr
  , trans :: AdjDerSubst dr sr -> Maybe (AdjDerSubst dr sr)
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
  , trans: \sigma -> execStateT (trans sigma) emptyRecordOfMaps
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
  , input :: MetaDer dr sr
  , trans :: AdjDerSubst dr sr -> Maybe (AdjDerSubst dr sr)
  , output :: MetaAdjDer dr sr
  }

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
matchDer (DerL dll1 sigma1 %% kids1) (dl2 %% kids2) = do
  dll1 ## V.case_
    #
      ( \_ dll1' -> do
          matchDerL (DerL dll1' sigma1) dl2
          List.zip kids1 kids2 # traverse_ (uncurry matchDer)
      )
    # V.on _metaVar (\x -> setMetaVar_Der x (todo "d2"))

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
    # V.on _metaVar (\x -> setMetaVar_Sort x sort2)

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
    # V.on _metaVar (\x -> setMetaVar_ChangeSort x ch2)

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
          # V.on _bdry (\_bdry2 -> pure unit)
      )
    # V.on _bdry
        ( \bdry1 -> l2 ## V.case_
            # (\_ _ -> pure unit)
            # V.on _bdry (\bdry2 -> matchBdry bdry1 bdry2)
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
    # V.on _metaVar (\x -> setMetaVar_AdjDer x a2)

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

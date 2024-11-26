module Pantograph.Language where

import Pantograph.Tree
import Prelude

import Control.Alternative (class Alternative)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Control.MonadPlus (guard)
import Control.Plus (empty)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, fold, foldM, intercalate, traverse_)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Number (e)
import Data.Ord.Generic (genericCompare)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as V
import Pantograph.Config as Config
import Pantograph.Debug as Debug
import Pantograph.MetaVar (MetaVar)
import Pantograph.MetaVar as MV
import Pantograph.Pretty (class Pretty, indent, pretty)
import Pantograph.RevList as RevList
import Pantograph.Utility (bug, expand1, unsafeCoerce_because, todo, uniqueList, (##))
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- MetaR
--------------------------------------------------------------------------------

type MetaR r = (metaVar :: MetaVar | r)

_metaVar = Proxy :: Proxy "metaVar"

makeMetaVar :: forall r. MetaVar -> TreeV (metaVar :: MetaVar | r)
makeMetaVar x = V.inj _metaVar x % []

defAndMakeMetaVar :: forall r. String -> MetaVar /\ TreeV (metaVar :: MetaVar | r)
defAndMakeMetaVar str = x /\ makeMetaVar x
  where
  x = MV.MetaVar str

makeMetaVar' :: forall r. String -> TreeV (metaVar :: MetaVar | r)
makeMetaVar' x = V.inj _metaVar (MV.MetaVar x) % []

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
data DerL dr sr = DerL (Variant dr) (MV.Subst (Sort sr))

type MetaDer dr sr = Tree (MetaDerL dr sr)
type MetaDerL dr sr = DerL (MetaR dr) (MetaR sr)

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

makeBaseDerL :: forall d dr sr. d -> MV.Subst (Sort sr) -> DerL (BaseDerR d dr) sr
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

getSort_AdjL :: forall dr sr. Pretty (Variant dr) => Ord (Variant dr) => Eq (Variant sr) => AdjL dr sr -> DerRules dr sr -> Sort sr
getSort_AdjL (DerL dr sigma) derRules =
  dr #
    V.on _bdry (\(Bdry _ ch) -> ch # outerEndpoint)
      (\dr' -> getSort_DerL (DerL dr' sigma) derRules)

getSort_Adj :: forall dr sr. Pretty (Variant dr) => Ord (Variant dr) => Eq (Variant sr) => Tree (AdjL dr sr) -> Map (Variant dr) (DerRule sr) -> Tree (Variant sr)
getSort_Adj (adjL %% _) = getSort_AdjL adjL

--------------------------------------------------------------------------------
-- Adj
--------------------------------------------------------------------------------

type Adj dr sr = Tree (AdjL dr sr)
type AdjL dr sr = DerL (bdry :: Bdry sr | dr) sr

type MetaAdj dr sr = Tree (MetaAdjL dr sr)
type MetaAdjL dr sr = AdjL (MetaR dr) (MetaR sr)

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

makeAdjBdryDownPlus :: forall sr dr f_l f_r. Foldable f_l => Foldable f_r => SortL sr -> f_l (Sort sr) -> Adj dr sr -> f_r (Sort sr) -> Adj dr sr
makeAdjBdryDownPlus l ls kid rs =
  DerL (V.inj _bdry $ Bdry Down $ (V.inj _plus (PlusChange $ Tooth l (RevList.fromFoldable ls) (List.fromFoldable rs))) % [ todo "kid_sort" ]) empty %
    [ kid ]

makeAdjBdryDownMinus :: forall sr dr f_l f_r. Foldable f_l => Foldable f_r => SortL sr -> f_l (Sort sr) -> Adj dr sr -> f_r (Sort sr) -> Adj dr sr
makeAdjBdryDownMinus l ls kid rs =
  DerL (V.inj _bdry $ Bdry Down $ (V.inj _minus (MinusChange $ Tooth l (RevList.fromFoldable ls) (List.fromFoldable rs))) % [ todo "kid_sort" ]) empty %
    [ kid ]

infixl 2 makeAdjBdryDownPlus as %+
infixl 2 makeAdjBdryDownMinus as %-

makeAdjBdryDown :: forall dr sr. ChangeSort sr -> Adj dr sr -> Adj dr sr
makeAdjBdryDown ch kid =
  DerL (V.inj _bdry (Bdry Down ch)) empty %
    [ kid ]

makeAdjBdryUp :: forall dr sr. ChangeSort sr -> Adj dr sr -> Adj dr sr
makeAdjBdryUp ch kid =
  DerL (V.inj _bdry (Bdry Up ch)) empty %
    [ kid ]

infix 2 makeAdjBdryDown as ↓
infix 2 makeAdjBdryUp as ↑

data AdjSubst dr sr = AdjSubst
  { adjs :: MV.Subst (Adj dr sr)
  , chs :: MV.Subst (ChangeSort sr)
  , sorts :: MV.Subst (Sort sr)
  }

instance
  ( Pretty (Adj dr sr)
  , Pretty (ChangeSort sr)
  , Pretty (Sort sr)
  ) =>
  Pretty (AdjSubst dr sr) where
  pretty (AdjSubst { adjs, chs, sorts }) =
    [ "adjs  : " <> pretty adjs -- pretty adjs
    , "chs   : " <> pretty chs
    , "sorts : " <> pretty sorts
    ] # intercalate "\n"

_adjs = Proxy :: Proxy "adjs"
_chs = Proxy :: Proxy "chs"
_sorts = Proxy :: Proxy "sorts"

emptyAdjSubst :: forall dr sr. AdjSubst dr sr
emptyAdjSubst = AdjSubst { adjs: empty, chs: empty, sorts: empty }

applyAdjSubst_Sort :: forall dr sr. AdjSubst dr sr -> MetaSort sr -> Sort sr
applyAdjSubst_Sort (sigma@(AdjSubst { sorts })) (l %% kids) =
  l ## V.case_
    # (\_ l' -> l' %% (kids # map (applyAdjSubst_Sort sigma)))
    # V.on _metaVar (\x -> sorts MV.!! x)

applyAdjSubst_ChangeSort :: forall dr sr. AdjSubst dr sr -> MetaChangeSort sr -> ChangeSort sr
applyAdjSubst_ChangeSort (sigma@(AdjSubst { chs })) (l %% kids) =
  l ## V.case_
    # (\_ l' -> l' %% (kids # map (applyAdjSubst_ChangeSort sigma)))
    # V.on _metaVar (\x -> chs MV.!! x)

applyAdjSubst_Adj :: forall dr sr. AdjSubst dr sr -> MetaAdj dr sr -> Adj dr sr
applyAdjSubst_Adj (sigma@(AdjSubst { adjs })) (DerL l sigma_d %% kids) =
  l ## V.case_
    #
      ( \_ l' -> DerL (unsafeCoerce_because "constant expansion of variant" l') (sigma_d <#> applyAdjSubst_Sort sigma) %%
          (kids # map (applyAdjSubst_Adj sigma))
      )
    # V.on _bdry
        ( \(Bdry dir ch) ->
            DerL (V.inj _bdry (Bdry dir (ch # applyAdjSubst_ChangeSort sigma))) (sigma_d <#> applyAdjSubst_Sort sigma) %%
              (kids # map (applyAdjSubst_Adj sigma))
        )
    # V.on _metaVar (\x -> adjs MV.!! x)

--------------------------------------------------------------------------------
-- AdjRules
--------------------------------------------------------------------------------

type AdjRules dr sr = List (AdjRule dr sr)

data AdjRule dr sr = AdjRule
  { input :: MetaAdj dr sr
  , trans :: AdjSubst dr sr -> Maybe (AdjSubst dr sr)
  , output :: MetaAdj dr sr
  }

-- instance (Show d, Show s, Show (AdjR dr sr)) => Show (AdjRule dr sr) where
--   show (AdjRule { input, trans: _, output }) =
--     -- "AdjRule { input: " <> show input <> ", output: " <> show output <> ", trans: <function>" <> " }"
--     todo ""

-- instance (Show d, Show s, PrettyDerL d, PrettyTreeL s) => Pretty (AdjRule dr sr) where
--   pretty (AdjRule { input, trans: _, output }) =
--     -- pretty input <> "  ~~>  " <> pretty output <> "  with  <function>"
--     todo ""

makeAdjRule input output trans = AdjRule { input, trans: trans >>> map \{ sorts, adjs, chs } -> AdjSubst { sorts: Map.fromFoldable sorts, adjs: Map.fromFoldable adjs, chs: Map.fromFoldable chs }, output }
makeSimpleAdjRule input output = AdjRule { input, trans: pure, output }

-- applyAdjRule
--   :: forall dr sr
--    . Eq (Variant dr)
--   => Eq (Variant (SortL s sr))
--   => Eq (Variant (ChangeSortR s sr))
--   => Eq (AdjR dr sr)
--   => IsLanguage d s
--   => AdjRule dr sr
--   -> Adj dr sr
--   -> Maybe (Adj dr sr)
-- applyAdjRule (AdjRule { input, output, trans }) adj = do
--   sigma_input <- adj # matchAdj input
--   sigma_output <- trans sigma_input
--   let output' = applyAdjSubst_Adj sigma_output output
--   -- Debug.logM 0 $ intercalate "\n"
--   --   [ "applyAdjRule.try match"
--   --   , "  - input        = " <> pretty input
--   --   , "  - adj          = " <> pretty adj
--   --   , "  - output       = " <> pretty output
--   --   , "  - sigma_output = " <> indent 2 (pretty sigma_output)
--   --   , "  - output' = " <> pretty output'
--   --   ]
--   pure output'

--------------------------------------------------------------------------------
-- EditRules
--------------------------------------------------------------------------------

type EditRules dr sr = List (EditRule dr sr)

data EditRule dr sr = EditRule
  { label :: String
  , input :: MetaDer dr sr
  , trans :: AdjSubst dr sr -> Maybe (AdjSubst dr sr)
  , output :: MetaAdj dr sr
  }

-- applyEditRule :: forall dr sr. EditRule dr sr -> Der dr sr -> Maybe (Adj dr sr)
-- applyEditRule (EditRule rule) dt = do
--   sigma <- matchDer rule.input dt # runMatchM
--   sigma' <- rule.trans sigma
--   pure $ applyAdjSubst_Adj sigma' rule.output

--------------------------------------------------------------------------------
-- Language
--------------------------------------------------------------------------------

data Language dr sr = Language
  { name :: String
  , derRules :: DerRules dr sr
  , adjRules :: AdjRules dr sr
  , editRules :: EditRules dr sr
  }

--------------------------------------------------------------------------------
-- match stuff
--------------------------------------------------------------------------------

type SortSubst sr = MV.Subst (Sort sr)
type MetaSortSubst sr = MV.Subst (MetaSort sr)

type MatchM dr sr = StateT (AdjSubst dr sr) Maybe Unit

runMatchM :: forall dr sr. MatchM dr sr -> Maybe (AdjSubst dr sr)
runMatchM = flip execStateT
  $ AdjSubst
      { adjs: Map.empty
      , chs: Map.empty
      , sorts: Map.empty
      }

setMetaVar_Sort :: forall dr sr. Eq (Sort sr) => MetaVar -> Sort sr -> MatchM dr sr
setMetaVar_Sort x sort = do
  AdjSubst sigma <- get
  case sigma.sorts # Map.lookup x of
    Nothing -> put $ AdjSubst sigma { sorts = sigma.sorts # Map.insert x sort }
    Just sort' | sort == sort' -> pure unit
    Just _ | otherwise -> empty

setMetaVar_ChangeSort :: forall dr sr. Eq (ChangeSort sr) => MetaVar -> ChangeSort sr -> MatchM dr sr
setMetaVar_ChangeSort x ch = do
  AdjSubst sigma <- get
  case sigma.chs # Map.lookup x of
    Nothing -> put $ AdjSubst sigma { chs = sigma.chs # Map.insert x ch }
    Just ch' | ch == ch' -> pure unit
    Just _ | otherwise -> empty

setMetaVar_Adj :: forall dr sr. Eq (Adj dr sr) => MetaVar -> Adj dr sr -> MatchM dr sr
setMetaVar_Adj x adj = do
  AdjSubst sigma <- get
  case sigma.adjs # Map.lookup x of
    Nothing -> put $ AdjSubst sigma { adjs = sigma.adjs # Map.insert x adj }
    Just adj' | adj == adj' -> pure unit
    Just _ | otherwise -> empty

matchSort :: forall dr sr. Eq (Variant sr) => MetaSort sr -> Sort sr -> MatchM dr sr
matchSort (msl1 %% kids1) sort2@(sl2 %% kids2) =
  msl1 ## V.case_
    #
      ( \_ sl1 -> do
          guard $ sl1 == sl2
          List.zip kids1 kids2 # traverse_ (uncurry matchSort)
      )
    # V.on _metaVar (\x -> setMetaVar_Sort x sort2)

matchSortSubst :: forall dr sr. Eq (Sort sr) => Eq (Variant sr) => MetaSortSubst sr -> SortSubst sr -> MatchM dr sr
matchSortSubst mss1 ss2 =
  Map.keys mss1 `Set.union` Map.keys ss2 # traverse_ \x -> do
    ms1 <- mss1 # Map.lookup x # lift
    s2 <- ss2 # Map.lookup x # lift
    matchSort ms1 s2

matchChangeSort :: forall dr sr. Eq (ChangeSort sr) => Eq (Variant (ChangeSortR sr)) => MetaChangeSort sr -> ChangeSort sr -> MatchM dr sr
matchChangeSort (mch1 %% kids1) ch2@(chl2 %% kids2) =
  mch1 ## V.case_
    #
      ( \_ chl1 -> do
          guard $ chl1 == chl2
          List.zip kids1 kids2 # traverse_ (uncurry matchChangeSort)
      )
    # V.on _metaVar (\x -> setMetaVar_ChangeSort x ch2)

-- matchDer :: forall dr sr. Eq (Variant (MetaR dr)) => Eq (Variant sr) => MetaDerL dr sr -> DerL dr sr -> MatchM dr sr
-- matchDer (DerL md1 sigma1) (DerL d2 sigma2) = do
--   guard $ md1 == ?d2
--   matchSortSubst sigma1 sigma2

-- matchDer :: forall dr sr. MetaDer dr sr -> Der dr sr -> MatchM dr sr
-- matchDer = todo "matchDer"

matchBdry :: forall dr sr. Eq (Variant (ChangeSortR sr)) => MetaBdry sr -> Bdry sr -> MatchM dr sr
matchBdry (Bdry dir1 ch1) (Bdry dir2 ch2) = do
  guard $ dir1 == dir2
  matchChangeSort ch1 ch2

-- matchAdjR
--   :: forall dr sr
--    . Eq (Variant dr)
--   => Eq (Variant (SortL s sr))
--   => Eq (Variant (ChangeSortR s sr))
--   => IsLanguage d s
--   => AdjR d dr s (MetaR sr)
--   -> AdjR dr sr
--   -> Maybe (AdjSubst dr sr)
-- matchAdjR a1 a2 =
--   a1 ## V.case_
--     -- #
--     --   ( \_ l1 -> a2 ## V.case_
--     --       #
--     --         ( \_ l2 -> do
--     --             guard $ (l1 :: Variant dr) == l2
--     --             pure emptyAdjSubst
--     --         )
--     --       # V.on _der (\_ -> empty)
--     --       # V.on _bdry (\_ -> empty)
--     --   )
--     -- # V.on _der
--     --     ( \der1 -> a2 ## V.case_
--     --         # (\_ _ -> empty)
--     --         # V.on _der (\der2 -> matchDer der1 der2 # runMatchM)
--     --     )
--     -- # V.on _bdry
--     --     ( \bdry1 -> a2 ## V.case_
--     --         # (\_ _ -> empty)
--     --         # V.on _bdry (\bdry2 -> matchBdry bdry1 bdry2 # runMatchM)
--     --     )
--     # todo ""

-- matchAdj
--   :: forall dr sr
--    . Eq (Variant dr)
--   => Eq (Variant (SortL s sr))
--   => Eq (Variant (ChangeSortR s sr))
--   => Eq (AdjR dr sr)
--   => IsLanguage d s
--   => MetaAdj dr sr
--   -> Adj dr sr
--   -> Maybe (AdjSubst dr sr)
-- matchAdj (DerL l_ma _ %% kids_ma) at@(l_a %% kids_a) =
--   l_ma ## V.case_
--     #
--       ( \_ l_ma' -> do
--           sigma <- matchAdjR l_ma' l_a
--           sigmas_kids <- (kids_ma `List.zip` kids_a) # traverse (uncurry matchAdj)
--           union_AdjSubst sigma =<< unions_AdjSubst sigmas_kids
--       )
--     -- # V.on _metaVar (\x -> pure $ AdjSubst { adjs: Map.singleton x at, chs: Map.empty, sorts: Map.empty })
--     # todo ""

-- union_AdjSubst
--   :: forall dr sr
--    . Eq (AdjR dr sr)
--   => Eq (Variant (ChangeSortR s sr))
--   => Eq (Variant (SortL s sr))
--   => IsLanguage d s
--   => AdjSubst dr sr
--   -> AdjSubst dr sr
--   -> Maybe (AdjSubst dr sr)
-- union_AdjSubst (AdjSubst sigma1) (AdjSubst sigma2) = do
--   adjs <- sigma1.adjs # Map.toUnfoldable # List.foldM insertIfEq sigma2.adjs
--   chs <- sigma1.chs # Map.toUnfoldable # List.foldM insertIfEq sigma2.chs
--   sorts <- sigma1.sorts # Map.toUnfoldable # List.foldM insertIfEq sigma2.sorts
--   pure $ AdjSubst { adjs, chs, sorts }

-- unions_AdjSubst
--   :: forall f dr sr
--    . Eq (AdjR dr sr)
--   => Eq (Variant (ChangeSortR s sr))
--   => Eq (Variant (SortL s sr))
--   => IsLanguage d s
--   => Foldable f
--   => f (AdjSubst dr sr)
--   -> Maybe (AdjSubst dr sr)
-- unions_AdjSubst = foldM union_AdjSubst (AdjSubst { adjs: Map.empty, chs: Map.empty, sorts: Map.empty })

insertIfEq :: forall m k v. Monad m => Alternative m => Ord k => Eq v => Map k v -> k /\ v -> m (Map k v)
insertIfEq sigma (x /\ a) = case sigma # Map.lookup x of
  Nothing -> pure $ sigma # Map.insert x a
  Just a' -> do
    guard $ a == a'
    pure sigma

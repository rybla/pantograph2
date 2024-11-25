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
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Show.Generic (genericShow)
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
import Pantograph.Utility (bug, expand1, todo, uniqueList, (##))
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- SortL
--------------------------------------------------------------------------------

type SortL :: Type -> Row Type -> Row Type
type SortL s l = (sort :: s | l)

_sort = Proxy :: Proxy "sort"

makeSort s kids = V.inj _sort s % kids

infix 3 makeSort as ^%

class (Eq s, Ord s, Show s, PrettyTreeL s) <= IsSortL s

--------------------------------------------------------------------------------
-- MetaL
--------------------------------------------------------------------------------

type MetaL l = (metaVar :: MetaVar | l)

_metaVar = Proxy :: Proxy "metaVar"

makeMetaVar :: forall l. MetaVar -> TreeV (metaVar :: MetaVar | l)
makeMetaVar x = V.inj _metaVar x % []

defAndMakeMetaVar :: forall l. String -> MetaVar /\ TreeV (metaVar :: MetaVar | l)
defAndMakeMetaVar str = x /\ makeMetaVar x
  where
  x = MV.MetaVar str

makeMetaVar' :: forall l. String -> TreeV (metaVar :: MetaVar | l)
makeMetaVar' x = V.inj _metaVar (MV.MetaVar x) % []

renameMVs :: forall l. (MetaVar -> MetaVar) -> TreeV (MetaL l) -> TreeV (MetaL l)
renameMVs f = map
  ( V.case_
      # (\_ l -> expand1 (Proxy :: Proxy "metaVar") l)
      # V.on _metaVar (\x -> V.inj _metaVar (f x))
  )

collectMVs :: forall l. TreeV (MetaL l) -> List MetaVar
collectMVs =
  map
    ( V.case_
        # mempty
        # V.on _metaVar pure
    )
    >>> fold
    >>> uniqueList

--------------------------------------------------------------------------------
-- DerL
--------------------------------------------------------------------------------

type DerL d dl s sl = (der :: Der d s sl | dl)

_der = Proxy :: Proxy "der"

data Der d s sl = Der d (MV.Subst (TreeV (SortL s sl)))

derive instance Generic (Der d s sl) _

instance (Eq d, Eq (Variant (SortL s sl))) => Eq (Der d s sl) where
  eq x = genericEq x

instance (Show d, Show (Variant (SortL s sl))) => Show (Der d s sl) where
  show x = genericShow x

class PrettyTreeDerL d where
  prettyTreeDerL :: d -> MV.Subst String -> List String -> String

instance (PrettyTreeDerL d, PrettyTreeL_R (SortL s sl)) => PrettyTreeL (Der d s sl) where
  prettyTreeL (Der d sigma) kids = prettyTreeDerL d (sigma # map pretty) kids

makeDer d sigma = V.inj _der $ Der d (sigma # Map.fromFoldable)

infix 5 makeDer as //

class (Eq d, Ord d, Show d, PrettyTreeDerL d) <= IsDerL d

--------------------------------------------------------------------------------
-- DerRule
--------------------------------------------------------------------------------

type DerRules d s = Map d (DerRule s)

class HasDerRules d s where
  derRules :: DerRules d s

data DerRule s = DerRule
  { sort :: TreeV (MetaL (SortL s ()))
  , kids :: List { sort :: TreeV (MetaL (SortL s ())) }
  }

_kids = Proxy :: Proxy "kids"

makeDerRule sort kids = DerRule
  { sort
  , kids: kids # map (\s -> { sort: s }) # List.fromFoldable
  }

infix 1 makeDerRule as -|

makeDerRuleFlipped = flip makeDerRule

infix 1 makeDerRuleFlipped as |-

class (IsDerL d, IsSortL s, HasDerRules d s) <= IsLanguage d s

--------------------------------------------------------------------------------
-- AdjL
--------------------------------------------------------------------------------

type AdjL d l_d s l_s = (bdry :: Bdry s l_s | DerL d l_d s l_s)

data Bdry s l_s = Bdry BdryDir (TreeV (ChangeL (SortL s l_s)))

derive instance Generic (Bdry s l_s) _

instance Eq (Variant (ChangeL (SortL s l_s))) => Eq (Bdry s l_s) where
  eq x = genericEq x

instance Show (Variant (ChangeL (SortL s l_s))) => Show (Bdry s l_s) where
  show x = genericShow x

instance PrettyTreeL_R (ChangeL (SortL s l_s)) => PrettyTreeL (Bdry s l_s) where
  prettyTreeL (Bdry dir ch) (kid : Nil) = "{{ " <> pretty ch <> " " <> pretty dir <> " " <> kid <> " }}"
  prettyTreeL (Bdry _ _) _ = bug "invalid Bdry"

_bdry = Proxy :: Proxy "bdry"

data BdryDir = Up | Down

derive instance Generic BdryDir _

derive instance Eq BdryDir

instance Show BdryDir where
  show x = genericShow x

instance Pretty BdryDir where
  pretty Up = "↑"
  pretty Down = "↓"

makeAdjBdryDownPlus l ls kid rs = V.inj _plus (PlusChange $ Tooth (V.inj _sort l) (RevList.fromFoldable ls) (List.fromFoldable rs)) % [ kid ]
makeAdjBdryDownMinus l ls kid rs = V.inj _minus (MinusChange $ Tooth (V.inj _sort l) (RevList.fromFoldable ls) (List.fromFoldable rs)) % [ kid ]

infixl 2 makeAdjBdryDownPlus as %+
infixl 2 makeAdjBdryDownMinus as %-

applyFunction :: forall a b. (a -> b) -> a -> b
applyFunction f a = f a

infixl 2 applyFunction as <<
infixl 2 applyFunction as >>

makeAdjBdryDown :: forall d s. ChT s -> AdjT d s -> AdjT d s
makeAdjBdryDown ch kid = V.inj _bdry (Bdry Down ch) % [ kid ]

infix 2 makeAdjBdryDown as ↓

makeAdjBdryUp :: forall d s. ChT s -> AdjT d s -> AdjT d s
makeAdjBdryUp ch kid = V.inj _bdry (Bdry Up ch) % [ kid ]

infix 2 makeAdjBdryUp as ↑

data AdjSubst d s = AdjSubst
  { adjs :: MV.Subst (AdjT d () s ())
  , chs :: MV.Subst (TreeV (ChangeL (SortL s ())))
  , sorts :: MV.Subst (TreeV (SortL s ()))
  }

instance IsLanguage d s => Pretty (AdjSubst d s) where
  pretty (AdjSubst { adjs, chs, sorts }) =
    [ "adjs  : " <> pretty adjs
    , "chs   : " <> pretty chs
    , "sorts : " <> pretty sorts
    ] # intercalate "\n"

_adjs = Proxy :: Proxy "adjs"
_chs = Proxy :: Proxy "chs"
_sorts = Proxy :: Proxy "sorts"

applyAdjSubst_SortT :: forall d s. AdjSubst d s -> MetaSortT s -> SortT s
applyAdjSubst_SortT (sigma@(AdjSubst { sorts })) (l %% kids) =
  l ## V.case_
    # (\_ l' -> l' %% (kids # map (applyAdjSubst_SortT sigma)))
    # V.on _metaVar (\x -> sorts MV.!! x)

applyAdjSubst_ChT :: forall d s. AdjSubst d s -> MetaChT s -> ChT s
applyAdjSubst_ChT (sigma@(AdjSubst { chs })) (l %% kids) =
  l ## V.case_
    # (\_ l' -> l' %% (kids # map (applyAdjSubst_ChT sigma)))
    # V.on _metaVar (\x -> chs MV.!! x)

applyAdjSubst_AdjT :: forall d s. AdjSubst d s -> MetaAdjT d s -> AdjT d s
applyAdjSubst_AdjT (sigma@(AdjSubst { adjs })) (l %% kids) =
  l ## V.match
    { metaVar: \x -> adjs MV.!! x
    , bdry: \(Bdry dir ch) ->
        V.inj _bdry (Bdry dir (ch # applyAdjSubst_ChT sigma)) %%
          (kids # map (applyAdjSubst_AdjT sigma))
    , der: \(Der d sigma_d) ->
        V.inj _der (Der d (sigma_d # (map (applyAdjSubst_SortT sigma)))) %%
          (kids # map (applyAdjSubst_AdjT sigma))
    }

--------------------------------------------------------------------------------
-- AdjRules
--------------------------------------------------------------------------------

type MetaAdjT d l_d s l_s = TreeV (MetaAdjL d l_d s l_s)
type MetaAdjL d l_d s l_s = MetaL ((AdjL s (MetaL l_s)) (DerL d l_d s (MetaL l_d)))

class (IsLanguage d s, HasAdjRules d s) <= IsAdjLanguage d s

class HasAdjRules d s where
  adjRules :: AdjRules d s

type AdjRules d s = List (AdjRule d s)

data AdjRule d s = AdjRule
  { input :: MetaAdjT d s
  , trans :: AdjSubst d s -> Maybe (AdjSubst d s)
  , output :: MetaAdjT d s
  }

instance (Show d, Show s) => Show (AdjRule d s) where
  show (AdjRule { input, trans: _, output }) =
    "AdjRule { input: " <> show input <> ", output: " <> show output <> ", trans: <function>" <> " }"

instance (Show d, Show s, PrettyTreeDerL d, PrettyTreeL s) => Pretty (AdjRule d s) where
  pretty (AdjRule { input, trans: _, output }) =
    pretty input <> "  ~~>  " <> pretty output <> "  with  <function>"

makeAdjRule input output trans = AdjRule { input, trans: trans >>> map \{ sorts, adjs, chs } -> AdjSubst { sorts: Map.fromFoldable sorts, adjs: Map.fromFoldable adjs, chs: Map.fromFoldable chs }, output }
makeSimpleAdjRule input output = AdjRule { input, trans: pure, output }

applyAdjRule :: forall d s. IsLanguage d s => AdjRule d s -> AdjT d s -> Maybe (AdjT d s)
applyAdjRule (AdjRule { input, output, trans }) adj = do
  sigma_input <- adj # matchAdjT input
  sigma_output <- trans sigma_input
  let output' = applyAdjSubst_AdjT sigma_output output
  Debug.logM 0 $ intercalate "\n"
    [ "applyAdjRule.try match"
    , "  - input        = " <> pretty input
    , "  - adj          = " <> pretty adj
    , "  - output       = " <> pretty output
    , "  - sigma_output =" <> indent 2 (pretty sigma_output)
    , "  - output' = " <> pretty output'
    ]
  pure output'

--------------------------------------------------------------------------------
-- EditRules
--------------------------------------------------------------------------------

class (IsAdjLanguage d s, HasEditRules d s) <= IsEditLanguage d s

class HasEditRules d s where
  editRules :: EditRules d s

type EditRules d s = List (EditRule d s)

data EditRule d s = EditRule
  { label :: String
  , input :: MetaDerT d s
  , trans :: AdjSubst d s -> Maybe (AdjSubst d s)
  , output :: MetaAdjT d s
  }

applyEditRule :: forall d s. IsLanguage d s => EditRule d s -> DerT d s -> Maybe (AdjT d s)
applyEditRule (EditRule rule) dt = do
  sigma <- matchDerT rule.input dt # runMatchM
  sigma' <- rule.trans sigma
  pure $ applyAdjSubst_AdjT sigma' rule.output

--------------------------------------------------------------------------------
-- match stuff
--------------------------------------------------------------------------------

type MatchM d s = StateT (AdjSubst d s) Maybe Unit

runMatchM :: forall d s. MatchM d s -> Maybe (AdjSubst d s)
runMatchM = flip execStateT
  $ AdjSubst
      { adjs: Map.empty
      , chs: Map.empty
      , sorts: Map.empty
      }

type SortSubst s = MV.Subst (TreeV (SortL s ()))
type MetaSortSubst s = MV.Subst (TreeV (MetaL (SortL s ())))

type MetaDerT d l_d s l_s = TreeV (MetaDerL d l_d s l_s)
type DerT d l_d s l_s = TreeV (DerL d l_d s l_s)

type MetaDerL d l_d s l_s = DerL d l_d s (MetaL l_s)

type MetaDer d l_s s = Der d s (MetaL l_s)

type MetaChT s l_s = TreeV (MetaL (ChangeL (SortL s l_s)))
type ChT s l_s = TreeV (ChangeL (SortL s l_s))

type AdjT d l_d s l_s = TreeV (AdjL d l_s s l_s)

type SortT s = TreeV (SortL s ())
type MetaSortT s = TreeV (MetaL (SortL s ()))

setMetaVar_Sort :: forall d s. IsLanguage d s => MetaVar -> SortT s -> MatchM d s
setMetaVar_Sort x sort = do
  AdjSubst sigma <- get
  case sigma.sorts # Map.lookup x of
    Nothing -> put $ AdjSubst sigma { sorts = sigma.sorts # Map.insert x sort }
    Just sort' | sort == sort' -> pure unit
    Just _ | otherwise -> empty

setMetaVar_Ch :: forall d s. IsLanguage d s => MetaVar -> ChT s -> MatchM d s
setMetaVar_Ch x ch = do
  AdjSubst sigma <- get
  case sigma.chs # Map.lookup x of
    Nothing -> put $ AdjSubst sigma { chs = sigma.chs # Map.insert x ch }
    Just ch' | ch == ch' -> pure unit
    Just _ | otherwise -> empty

setMetaVar_Adj :: forall d s. IsLanguage d s => MetaVar -> AdjT d s -> MatchM d s
setMetaVar_Adj x adj = do
  AdjSubst sigma <- get
  case sigma.adjs # Map.lookup x of
    Nothing -> put $ AdjSubst sigma { adjs = sigma.adjs # Map.insert x adj }
    Just adj' | adj == adj' -> pure unit
    Just _ | otherwise -> empty

matchSort :: forall d s. IsLanguage d s => MetaSortT s -> SortT s -> MatchM d s
matchSort (msl1 %% kids1) sort2@(sl2 %% kids2) =
  msl1 ## V.case_
    #
      ( \_ sl1 -> do
          guard $ sl1 == sl2
          List.zip kids1 kids2 # traverse_ (uncurry matchSort)
      )
    # V.on _metaVar (\x -> setMetaVar_Sort x sort2)

matchSortSubst :: forall d s. IsLanguage d s => MetaSortSubst s -> SortSubst s -> MatchM d s
matchSortSubst mss1 ss2 =
  Map.keys mss1 `Set.union` Map.keys ss2 # traverse_ \x -> do
    ms1 <- mss1 # Map.lookup x # lift
    s2 <- ss2 # Map.lookup x # lift
    matchSort ms1 s2

matchCh :: forall d s. IsLanguage d s => MetaChT s -> ChT s -> MatchM d s
matchCh (mch1 %% kids1) ch2@(chl2 %% kids2) =
  mch1 ## V.case_
    #
      ( \_ chl1 -> do
          guard $ chl1 == chl2
          List.zip kids1 kids2 # traverse_ (uncurry matchCh)
      )
    # V.on _metaVar (\x -> setMetaVar_Ch x ch2)

matchDer :: forall d s. IsLanguage d s => MetaDer d s -> Der d s () -> MatchM d s
matchDer (Der md1 sigma1) (Der d2 sigma2) = do
  guard $ md1 == d2
  matchSortSubst sigma1 sigma2

matchDerT :: forall d s. IsLanguage d s => MetaDerT d s -> DerT d s -> MatchM d s
matchDerT = todo "matchDerT"

matchAdjL
  :: forall d s
   . IsLanguage d s
  => Variant (AdjL d () s (MetaL ()))
  -> Variant (AdjL d () s ())
  -> Maybe (AdjSubst d s)
matchAdjL a1 a2 =
  a1 # V.match
    { bdry: \(Bdry dir1 ch1) ->
        a2 # V.match
          { bdry: \(Bdry dir2 ch2) -> do
              guard $ dir1 == dir2
              matchCh ch1 ch2 # runMatchM
          , der: const empty
          }
    , der: \der1 ->
        a2 # V.match
          { bdry: const empty
          , der: \der2 -> matchDer der1 der2 # runMatchM
          }
    }

matchAdjT :: forall d s. IsLanguage d s => MetaAdjT d s -> AdjT d s -> Maybe (AdjSubst d s)
matchAdjT (l_ma %% kids_ma) at@(l_a %% kids_a) =
  V.case_
    #
      ( \_ l_ma' -> do
          sigma <- matchAdjL l_ma' l_a
          sigmas_kids <- (kids_ma `List.zip` kids_a) # traverse (uncurry matchAdjT)
          union_AdjSubst sigma =<< unions_AdjSubst sigmas_kids
      )
    # V.on _metaVar (\x -> pure $ AdjSubst { adjs: Map.singleton x at, chs: Map.empty, sorts: Map.empty })
    $ l_ma

union_AdjSubst :: forall d s. IsLanguage d s => AdjSubst d s -> AdjSubst d s -> Maybe (AdjSubst d s)
union_AdjSubst (AdjSubst sigma1) (AdjSubst sigma2) = do
  adjs <- sigma1.adjs # Map.toUnfoldable # List.foldM insertIfEq sigma2.adjs
  chs <- sigma1.chs # Map.toUnfoldable # List.foldM insertIfEq sigma2.chs
  sorts <- sigma1.sorts # Map.toUnfoldable # List.foldM insertIfEq sigma2.sorts
  pure $ AdjSubst { adjs, chs, sorts }

unions_AdjSubst :: forall f d s. IsLanguage d s => Foldable f => f (AdjSubst d s) -> Maybe (AdjSubst d s)
unions_AdjSubst = foldM union_AdjSubst (AdjSubst { adjs: Map.empty, chs: Map.empty, sorts: Map.empty })

insertIfEq :: forall m k v. Monad m => Alternative m => Ord k => Eq v => Map k v -> k /\ v -> m (Map k v)
insertIfEq sigma (x /\ a) = case sigma # Map.lookup x of
  Nothing -> pure $ sigma # Map.insert x a
  Just a' -> do
    guard $ a == a'
    pure sigma

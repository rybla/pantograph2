module Pantograph.Language where

import Pantograph.Tree
import Prelude

import Control.Alternative (class Alternative, empty)
import Control.MonadPlus (class MonadPlus, guard)
import Control.Plus (empty)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, fold, foldM, foldr)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as V
import Pantograph.MetaVar (MetaVar)
import Pantograph.MetaVar as MetaVar
import Pantograph.Pretty (class Pretty, pretty)
import Pantograph.RevList as RevList
import Pantograph.Utility (bug, expand1, todo, uniqueList)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- SortL
--------------------------------------------------------------------------------

type SortL :: Type -> Row Type -> Row Type
type SortL s l = (sort :: s | l)

_sort = Proxy :: Proxy "sort"

makeSort s kids = V.inj _sort s % kids

infix 3 makeSort as %^

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
  x = MetaVar.MetaVar str

makeMetaVar' :: forall l. String -> TreeV (metaVar :: MetaVar | l)
makeMetaVar' x = V.inj _metaVar (MetaVar.MetaVar x) % []

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

type DerL d sl l = (der :: Der d sl | l)

_der = Proxy :: Proxy "der"

data Der d sl = Der d (MetaVar.Subst (TreeV sl))

derive instance Generic (Der d sl) _

instance (Eq d, Eq (Variant sl)) => Eq (Der d sl) where
  eq x = genericEq x

instance (Show d, Show (Variant sl)) => Show (Der d sl) where
  show x = genericShow x

instance PrettyTreeL d => PrettyTreeL (Der d sl) where
  prettyTreeL (Der d _sigma) kids = prettyTreeL d kids

makeDer d sigma = V.inj _der $ Der d (sigma # Map.fromFoldable)

infix 5 makeDer as //

class (Eq d, Ord d, Show d, PrettyTreeL d) <= IsDerL d

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

type AdjL ch l = (bdry :: Bdry ch | l)

data Bdry ch = Bdry BdryDir (TreeV ch)

derive instance Generic (Bdry ch) _

instance Eq (Variant ch) => Eq (Bdry ch) where
  eq x = genericEq x

instance Show (Variant ch) => Show (Bdry ch) where
  show x = genericShow x

instance PrettyTreeL_R ch => PrettyTreeL (Bdry ch) where
  prettyTreeL (Bdry dir ch) (kid : Nil) = pretty ch <> " " <> pretty dir <> " " <> pretty kid
  prettyTreeL (Bdry dir ch) _ = bug "invalid Bdry"

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

applyFunction f a = f a

infixl 2 applyFunction as <<
infixl 2 applyFunction as >>

makeAdjBdryDown :: forall l ch. TreeV ch -> TreeV (AdjL ch l) -> TreeV (AdjL ch l)
makeAdjBdryDown ch kid = V.inj _bdry (Bdry Down ch) % [ kid ]

infix 2 makeAdjBdryDown as ↓

makeAdjBdryUp :: forall l ch. TreeV ch -> TreeV (AdjL ch l) -> TreeV (AdjL ch l)
makeAdjBdryUp ch kid = V.inj _bdry (Bdry Up ch) % [ kid ]

infix 2 makeAdjBdryUp as ↑

data AdjSubst d s = AdjSubst
  { adjs :: MetaVar.Subst (AdjT d s)
  , chs :: MetaVar.Subst (TreeV (ChangeL (SortL s ())))
  , sorts :: MetaVar.Subst (TreeV (SortL s ()))
  }

_adjs = Proxy :: Proxy "adjs"
_chs = Proxy :: Proxy "chs"
_sorts = Proxy :: Proxy "sorts"

--------------------------------------------------------------------------------
-- AdjRules
--------------------------------------------------------------------------------

type MetaAdjT d s = TreeV (MetaAdjL d s)
type MetaAdjL d s = MetaL (AdjL (MetaL (ChangeL (SortL s ()))) (DerL d (MetaL (SortL s ())) ()))

class (IsLanguage d s, HasAdjRules d s) <= IsAdjLanguage d s

class HasAdjRules d s where
  adjRules :: AdjRules d s

type AdjRules d s = List (AdjRule d s)

data AdjRule d s = AdjRule
  { input :: MetaAdjT d s
  , trans :: AdjSubst d s -> Maybe (AdjSubst d s)
  , output :: MetaAdjT d s
  }

makeAdjRule input output trans = AdjRule { input, trans: trans >>> map \{ sorts, adjs, chs } -> AdjSubst { sorts: Map.fromFoldable sorts, adjs: Map.fromFoldable adjs, chs: Map.fromFoldable chs }, output }
makeSimpleAdjRule input output = AdjRule { input, trans: pure, output }

applyAdjRule :: forall d s. AdjRule d s -> AdjT d s -> Maybe (AdjT d s)
applyAdjRule = todo "applyAdjRule"

--------------------------------------------------------------------------------
-- EditRules
--------------------------------------------------------------------------------

class (IsAdjLanguage d s, HasEditRules d s) <= IsEditLanguage d s

class HasEditRules d s where
  editRules :: EditRules d s

type EditRules d s = List (EditRule d s)

data EditRule d s = EditRule
  { label :: String
  , input :: MetaAdjT d s
  , trans :: AdjSubst d s -> Maybe (AdjSubst d s)
  , output :: MetaAdjT d s
  }

--------------------------------------------------------------------------------
-- match stuff
--------------------------------------------------------------------------------

type SortSubst s = MetaVar.Subst (TreeV (SortL s ()))
type MetaSortSubst s = MetaVar.Subst (TreeV (MetaL (SortL s ())))

type MetaDerT d s = TreeV (MetaDerL d s)
type DerT d s = TreeV (DerL d (SortL s ()) ())

type MetaDerL d s = DerL d (MetaL (SortL s ())) ()

type MetaDer d s = Der d (MetaL (SortL s ()))

type MetaChT s = MetaL (ChangeL (SortL s ()))
type ChT s = ChangeL (SortL s ())

type AdjT d s = TreeV (AdjL (ChangeL (SortL s ())) (DerL d (SortL s ()) ()))

type SortT s = TreeV (SortL s ())
type MetaSortT s = TreeV (MetaL (SortL s ()))

matchSort :: forall d s. IsLanguage d s => MetaSortT s -> SortT s -> Maybe (AdjSubst d s)
matchSort = todo "matchSort"

matchSortSubst :: forall d s. IsLanguage d s => MetaSortSubst s -> SortSubst s -> Maybe (AdjSubst d s)
matchSortSubst = todo "matchSortSubst"

matchChT :: forall d s. IsLanguage d s => TreeV (MetaChT s) -> TreeV (ChT s) -> Maybe (AdjSubst d s)
matchChT = todo "matchChT"

matchDer :: forall d s. IsLanguage d s => MetaDer d s -> Der d (SortL s ()) -> Maybe (AdjSubst d s)
matchDer (Der d1 sigma1) (Der d2 sigma2) = do
  guard $ d1 == d2
  matchSortSubst sigma1 sigma2

matchAdjL
  :: forall d s
   . IsLanguage d s
  => Variant (AdjL (MetaL (ChangeL (SortL s ()))) (DerL d (MetaL (SortL s ())) ()))
  -> Variant (AdjL (ChangeL (SortL s ())) (DerL d (SortL s ()) ()))
  -> Maybe (AdjSubst d s)
matchAdjL a1 a2 =
  a1 # V.match
    { bdry: \(Bdry dir1 ch1) -> a2 # V.match
        { bdry: \(Bdry dir2 ch2) -> do
            guard $ dir1 == dir2
            matchChT ch1 ch2
        , der: const empty
        }
    -- , der: \(Der d1 sigma1) -> a2 # V.match
    --     { bdry: const empty
    --     , der: \(Der d2 sigma2) -> do
    --         guard $ d1 == d2
    --         matchSortSubst sigma1 sigma2
    --     }
    , der: \der1 -> a2 # V.match
        { bdry: const empty
        , der: \der2 -> matchDer der1 der2
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

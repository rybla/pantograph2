module Pantograph.Utility where

import Prelude

import Control.Alt (alt)
import Control.Alternative (empty)
import Control.Monad.Error.Class (throwError)
import Control.Plus (class Plus)
import Data.Either (Either)
import Data.Function (applyFlipped)
import Data.Function as Function
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.RowList as RowList
import Record as R
import Record as Record
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

infixr 1 Function.apply as $$

infixl 0 applyFlipped as ##

unsafeCoerce_because :: forall a b. String -> a -> b
unsafeCoerce_because _ = unsafeCoerce

todo :: forall a. String -> a
todo msg = unsafeCrashWith $ "TODO: " <> msg

unimplemented :: forall a. String -> a
unimplemented label = unsafeCrashWith $ "unimplemented: " <> label

bug :: forall b13. String -> b13
bug msg = unsafeCrashWith $ "bug: " <> msg

assert :: forall t21. String -> Boolean -> (Unit -> t21) -> t21
assert msg b k = if not b then bug msg else k unit

assertM :: forall m. Monad m => String -> Boolean -> m Unit
assertM msg b = if not b then pure (bug msg) else pure unit

fixpoint :: forall a. (a -> Maybe a) -> a -> a
fixpoint f a = case f a of
  Nothing -> a
  Just a' -> fixpoint f a'

tryFirst :: forall m a b. Plus m => (a -> m b) -> List a -> m b
tryFirst f = List.foldr (f >>> alt) empty

uniqueList :: forall a. Ord a => List a -> List a
uniqueList = Set.fromFoldable >>> Set.toUnfoldable

expand1 :: forall x a l l'. Cons x a l l' => Proxy x -> Variant l -> Variant l'
expand1 _ = unsafeCoerce

expand1' :: forall @x a l l'. Cons x a l l' => Variant l -> Variant l'
expand1' = unsafeCoerce

--------------------------------------------------------------------------------
-- FromObjectToRecord
--------------------------------------------------------------------------------

class FromObjectToRecord a r where
  fromObjectToRecord :: Object a -> Maybe (Record r)

instance (RowToList r rl, FromObjectToRecord' a r rl) => FromObjectToRecord a r where
  fromObjectToRecord = fromObjectToRecord' (Proxy @rl)

class FromObjectToRecord' a r (rl :: RowList Type) | rl -> r where
  fromObjectToRecord' :: Proxy rl -> Object a -> Maybe (Record r)

instance
  ( IsSymbol k
  , Cons k a r r'
  , Lacks k r
  , FromObjectToRecord' a r rl_
  ) =>
  FromObjectToRecord' a r' (RowList.Cons k v rl_) where
  fromObjectToRecord' _ o = do
    v <- o # Object.lookup (reflectSymbol (Proxy @k))
    r :: Record r <- fromObjectToRecord' (Proxy @rl_) o
    pure $ Record.insert (Proxy @k) v r

instance FromObjectToRecord' a () RowList.Nil where
  fromObjectToRecord' _ _ = pure {}

--------------------------------------------------------------------------------
-- FromMapToRecord
--------------------------------------------------------------------------------

class FromMapToRecord r a | r -> a where
  fromMapToRecord :: Map String a -> Either String (Record r)

class FromMapToRecord' :: Row Type -> RowList Type -> Type -> Constraint
class FromMapToRecord' r rl a | rl -> r, r -> a where
  fromMapToRecord' :: Proxy rl -> Map String a -> Either String (Record r)

instance
  ( IsSymbol k
  , Cons k a r r'
  , Lacks k r
  , FromMapToRecord' r rl_ a
  ) =>
  FromMapToRecord' r' (RowList.Cons k v rl_) a where
  fromMapToRecord' _ m =
    case m # Map.lookup k of
      Nothing -> throwError k
      Just v -> do
        r <- fromMapToRecord' (Proxy @rl_) m
        r # Record.insert _k v # pure
    where
    _k = (Proxy @k)
    k = reflectSymbol _k

instance FromMapToRecord' () RowList.Nil a where
  fromMapToRecord' _ _ = pure {}

--------------------------------------------------------------------------------
-- IsRecordOfMaps 
--------------------------------------------------------------------------------

class IsRecordOfMaps r where
  emptyRecordOfMaps :: Record r

instance (RowToList r rl, IsRecordOfMaps_RL r rl) => IsRecordOfMaps r where
  emptyRecordOfMaps = emptyRecordOfMaps_RL (Proxy @rl) (Proxy @r)

class IsRecordOfMaps_RL (r :: Row Type) (rl :: RowList Type) | rl -> r where
  emptyRecordOfMaps_RL :: Proxy rl -> Proxy r -> Record r

instance IsRecordOfMaps_RL () RL.Nil where
  emptyRecordOfMaps_RL _ _ = {}

instance
  ( IsSymbol x
  , Lacks x r
  , Cons x (Map k v) r r'
  , IsRecordOfMaps_RL r rl
  ) =>
  IsRecordOfMaps_RL r' (RL.Cons x (Map k v) rl) where
  emptyRecordOfMaps_RL _ _ =
    R.insert (Proxy @x) Map.empty
      $ emptyRecordOfMaps_RL (Proxy @rl) (Proxy @r)


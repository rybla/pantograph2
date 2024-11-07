module Pantograph.Utility where

import Prelude

import Control.Alternative (empty)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons, class Lacks)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record as Record
import Type.Prelude (Proxy(..))

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

tryFirst :: forall a b. (a -> Maybe b) -> List a -> Maybe b
tryFirst _ Nil = empty
tryFirst f (x : xs) = case f x of
  Nothing -> tryFirst f xs
  Just y -> pure y

--------------------------------------------------------------------------------
-- FromObjectToRecord
--------------------------------------------------------------------------------

class FromObjectToRecord a r where
  fromObjectToRecord :: Object a -> Maybe (Record r)

instance (RowToList r rl, FromObjectToRecord' a r rl) => FromObjectToRecord a r where
  fromObjectToRecord = fromObjectToRecord' (Proxy :: Proxy rl)

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
    v <- o # Object.lookup (reflectSymbol (Proxy :: Proxy k))
    r :: Record r <- fromObjectToRecord' (Proxy :: Proxy rl_) o
    pure $ Record.insert (Proxy :: Proxy k) v r

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
        r <- fromMapToRecord' (Proxy :: Proxy rl_) m
        r # Record.insert _k v # pure
    where
    _k = (Proxy :: Proxy k)
    k = reflectSymbol _k

instance FromMapToRecord' () RowList.Nil a where
  fromMapToRecord' _ _ = pure {}

--------------------------------------------------------------------------------
-- TypeList
--------------------------------------------------------------------------------

foreign import data TypeList :: Type
foreign import data NilTL :: TypeList
foreign import data ConsTL :: Type -> TypeList -> TypeList

infixr 1 type ConsTL as :*

class HeadTL :: TypeList -> Type -> Constraint
class HeadTL ts t | ts -> t

instance HeadTL (t :* ts) t

-- class HeadTL :: TypeList -> Type -> Type -> Constraint
-- class HeadTL ts t_default t | ts t_default -> t

-- instance HeadTL NilTL t_default t_default
-- else instance HeadTL (t :* ts) t_default t

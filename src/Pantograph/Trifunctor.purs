module Pantograph.Trifunctor where

import Prelude

class Trifunctor f where
  trimap :: forall a a' b b' c c'. (a -> a') -> (b -> b') -> (c -> c') -> f a b c -> f a' b' c'

map1 :: forall f a a' b' c'. Trifunctor f => (a -> a') -> f a b' c' -> f a' b' c'
map1 f = trimap f identity identity

map2 :: forall f a' b b' c'. Trifunctor f => (b -> b') -> f a' b c' -> f a' b' c'
map2 f = trimap identity f identity

map3 :: forall f a' b' c c'. Trifunctor f => (c -> c') -> f a' b' c -> f a' b' c'
map3 f = trimap identity identity f

data Trio a b c = Trio a b c

instance Trifunctor Trio where
  trimap f g h (Trio a b c) = Trio (f a) (g b) (h c)

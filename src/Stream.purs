module Stream
       ( StreamCell(..)
       , Stream(..)
       , appendStream
       , take
       , drop
       , reverse
       , prettyStream
       )
       where

import Prelude
import Data.Lazy (defer, force, Lazy)
import Data.Monoid (class Monoid)

data StreamCell a = Nil | Cons a (Stream a)
newtype Stream a = Stream (Lazy (StreamCell a))

test :: Stream Int
test =
  sdefer \_ ->
           Cons 1 (sdefer \_ ->
                    Cons 2 (sdefer \_ ->
                             Cons 3 (sdefer \_ ->
                                      Nil)))

appendStream :: forall a. Stream a -> Stream a -> Stream a
appendStream s t = sdefer \_ ->
  case sforce s of
    Nil -> sforce t
    Cons x sP -> Cons x (sP `appendStream` t)

take :: forall a. Int -> Stream a -> Stream a
take n s = sdefer \_ ->
  case n, sforce s of
    0, _ -> Nil
    _, Nil -> Nil
    _, Cons h t -> Cons h (take (n - 1) t)

drop :: forall a. Int -> Stream a -> Stream a
drop 0 s = s
drop n s =
  case n, sforce s of
    _, Nil -> sdefer \_ -> Nil
    _, Cons h t -> drop (n - 1) t

reverse :: forall a. Stream a -> Stream a
reverse s = sdefer \_ -> reverseP s Nil
  where
    reverseP s acc =
      case sforce s of
        Nil -> acc
        Cons h t -> reverseP t (Cons h (sdefer \_ -> acc))

prettyStream :: forall a. (a -> String) -> Stream a -> String
prettyStream formatter s =
  case sforce s of
    Nil -> "Nil"
    Cons h t -> formatter h <> ", " <> prettyStream formatter t

instance showStream :: Show a => Show (Stream a) where
  show = prettyStream show

instance semigroupStream :: Semigroup (Stream a) where
  append = appendStream

instance monoidStream :: Monoid (Stream a) where
  mempty = sdefer \_ -> Nil

sdefer :: forall a. (Unit -> StreamCell a) -> Stream a
sdefer = Stream <<< defer

sforce :: forall a. Stream a -> StreamCell a
sforce (Stream s) = force s

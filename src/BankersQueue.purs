module BankersQueue where

import Prelude
import Data.Maybe (Maybe(..))
import Stream (sdefer, sforce, StreamCell(Cons, Nil), reverse, Stream)

data Queue a = Queue { f :: Stream a, lenF :: Int, r :: Stream a, lenR :: Int }

queue :: forall a. { f :: Stream a, lenF :: Int, r :: Stream a, lenR :: Int } -> Queue a
queue q@{ f, lenF, r, lenR } =
  if lenR <= lenF then
    Queue q
  else
    Queue {f: f <> reverse r, lenF: lenF + lenR, r: sdefer \_ -> Nil, lenR: 0}

empty :: forall a. Queue a
empty = Queue {f: sdefer \_ -> Nil, lenF: 0, r: sdefer \_ -> Nil, lenR: 0}

isEmpty :: forall a. Queue a -> Boolean
isEmpty (Queue {lenF}) = lenF == 0

head :: forall a. Queue a -> Maybe a
head (Queue {f}) =
  case sforce f of
    Nil -> Nothing
    Cons h _ -> Just h

tail :: forall a. Queue a -> Maybe (Queue a)
tail (Queue {f, lenF, r, lenR}) =
  case sforce f of
    Nil -> Nothing
    Cons h t -> Just (queue {f: t, lenF: lenF - 1, r, lenR})

snoc :: forall a. Queue a -> a -> Queue a
snoc (Queue {f, lenF, r, lenR}) a =
  queue {f, lenF, r: sdefer \_ -> Cons a r, lenR: lenR + 1 }

prettyQueue :: forall a. (a -> String) -> Queue a -> String
prettyQueue formatter q =
  case head q, tail q of
    Just h, Just t -> formatter h <> ", " <> prettyQueue formatter t
    _, _ -> ""

instance showQueue :: Show a => Show (Queue a) where
  show = prettyQueue show

module BatchedQueue
       ( Queue
       , empty
       , isEmpty
       , snoc
       , head
       , tail
       , prettyQueue
       ) where

import Prelude
import Data.List (reverse, (:), List(..))
import Data.Maybe (Maybe(..))

newtype Queue a = Queue {f :: List a, r :: List a}

empty :: forall a. Queue a
empty = Queue {f: Nil, r : Nil}

isEmpty :: forall a. Queue a -> Boolean
isEmpty (Queue {f, r}) =
  case f, r of
    Nil, Nil -> true
    _, _ -> false

snoc :: forall a. Queue a -> a -> Queue a
snoc (Queue {f, r}) a = queue {f, r: a : r}

head :: forall a. Queue a -> Maybe a
head (Queue {f, r}) = case f of
  Nil -> Nothing
  h : _ -> Just h

tail :: forall a. Queue a -> Maybe (Queue a)
tail (Queue {f, r}) = case f of
  Nil -> Nothing
  _ : t -> Just (queue {f: t, r})

prettyQueue :: forall a. (a -> String) -> Queue a -> String
prettyQueue formatter q =
  case uncons q of
    Nothing -> ""
    Just {h, t} -> formatter h <> ", " <> prettyQueue formatter t

instance showQueue :: Show a => Show (Queue a) where
  show = prettyQueue show

-- private

queue :: forall a. {f :: List a, r :: List a} -> Queue a
queue {f, r} =
  Queue case f of
    Nil -> {f: reverse r, r: Nil}
    _ -> {f, r}

uncons :: forall a. Queue a -> Maybe {h :: a, t :: Queue a}
uncons q =
  {h: _, t: _} <$> head q <*> tail q

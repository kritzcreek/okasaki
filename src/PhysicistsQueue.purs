module PhysicistsQueue
  ( Queue
  , empty
  , isEmpty
  , checkW
  , checkR
  , head
  , tail
  , snoc
  , queue
  ) where

import Prelude
import Data.Lazy (defer, force, Lazy)
import Data.List ((:), reverse, List(..))
import Data.List as List
import Data.Maybe (fromJust, Maybe(..))
import Partial.Unsafe (unsafePartial)

newtype Queue a = Queue
  { w :: List a
  , f :: Lazy (List a)
  , lenF :: Int
  , r :: List a
  , lenR :: Int
  }

empty :: forall a. Queue a
empty = Queue {w: Nil, f: defer \_ -> Nil, lenF: 0, r: Nil, lenR: 0}

isEmpty :: forall a. Queue a -> Boolean
isEmpty (Queue {lenF}) = lenF == 0

checkW :: forall a. Queue a -> Queue a
checkW (Queue {w: Nil, f, lenF, r, lenR}) =
  Queue {w: force f, f, lenF, r, lenR}
checkW q = q

checkR :: forall a. Queue a -> Queue a
checkR q@(Queue {w, f, lenF, r, lenR}) =
  if lenR <= lenF then
    q
  else
    let w' = force f
    in Queue {w: w', f: defer \_ -> w' <> reverse r, lenF: lenF + lenR, r: Nil, lenR: 0 }

queue :: forall a. Queue a -> Queue a
queue q = checkW (checkR q)

snoc :: forall a. Queue a -> a -> Queue a
snoc (Queue { w, f, lenF, lenR, r }) a =
  queue (Queue { w, f, lenF, r: a : r, lenR: lenR + 1 })

head :: forall a. Queue a -> Maybe a
head (Queue { w: Nil }) = Nothing
head q@(Queue { w: Cons a _}) = Just a

tail :: forall a. Queue a -> Maybe (Queue a)
tail (Queue { w: Nil }) = Nothing
tail (Queue { w: Cons _ as, f, lenF, lenR, r }) =
  Just (queue (Queue { w: as
                     , f: defer \_ -> unsafePartial fromJust (List.tail (force f))
                     , lenF: lenF - 1
                     , r: r
                     , lenR: lenR
                     }))

-- tail

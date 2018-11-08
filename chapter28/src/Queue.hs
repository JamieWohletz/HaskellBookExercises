module Queue where

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

mkQueue :: Queue a
mkQueue = Queue [] []

push :: a -> Queue a -> Queue a
push a (Queue enq deq) = Queue { enqueue = a:enq, dequeue = deq }

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue enq deq) =
  case deq of
    [] ->
      if length enq > 0 then
        let (x:xs) = reverse enq
        in  Just (x, Queue [] xs)
      else
        Nothing
    (x:xs) -> Just (x, Queue enq xs)
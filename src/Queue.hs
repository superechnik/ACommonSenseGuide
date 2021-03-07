{-# LANGUAGE FlexibleInstances #-}
module Queue 
(dequeue
,enqueue
,peek
,fromList
,QueueClass
,Queue 
)where

type Queue a = [a]

class QueueClass a where 
    dequeue :: Queue a -> (a, Queue a)
    enqueue :: a -> Queue a -> Queue a
    peek :: Queue a -> a
    fromList :: [a] -> Queue a

instance QueueClass a where 
    dequeue (x:xs) = (x,xs)
    
    enqueue val ls = ls ++ [val]
    
    peek ls = head ls

    fromList ls = ls
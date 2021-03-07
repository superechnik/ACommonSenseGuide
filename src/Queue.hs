{-# LANGUAGE FlexibleInstances #-}
module Queue 
(dequeue
,enqueue
,peek
)where

class Queue a where 
    dequeue :: [a] -> (a, [a])
    enqueue :: a -> [a] -> [a]
    peek :: [a] -> a

instance Queue a where 
    dequeue (x:xs) = (x,xs)
    enqueue val ls = ls ++ [val]
    peek ls = ls !! (length ls - 1)
{-
This chapter is about stacks and queues. Question 1 is not about code, but 
questions 2,3,4 can be demonstrated with code. 
-}
module Chapter9
(popTwice 
,dequeueTwice
,reverseStringWithStack
)where

import Queue as Q 
import Stack as S


--Excercise 2
{-
if you push 1,2,3,4,5,6 onto a stack and popped twice, what number 
could you read from the stack
-}

popTwice :: S.Stack a -> a 
popTwice stack = 
    let once = snd $ S.pop stack 
        twice = snd $ S.pop once 
    in  S.peek twice

--Excercise 3
{-
if you enqueue 1,2,3,4,5,6 onto a queue and dequeue twice, what number
could you read from the queue
-}
dequeueTwice :: Q.Queue a -> a 
dequeueTwice queue = 
    let once = snd $ Q.dequeue queue 
        twice = snd $ Q.dequeue once 
    in  Q.peek twice


--Excercise 4
{-
Use a stack to reverse a string
-}
reverseStringWithStack :: String -> String 
reverseStringWithStack [] = []
reverseStringWithStack str = 
    let stack = S.fromList str 
        popped = S.pop stack 
    in  fst popped : reverseStringWithStack (snd popped)
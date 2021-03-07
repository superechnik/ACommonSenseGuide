{-
This chapter is about stacks and queues. Question 1 is not about code, but 
questions 2,3,4 can be demonstrated with code. 
-}
module Chapter9
(popTwice 
)where

import Queue as Q (enqueue,dequeue,peek)
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
--Excercise 4




    
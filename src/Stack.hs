{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Stack 
(pop
,push
,peek
,StackClass
,Stack
,fromList
)where 

import Data.Maybe

type Stack a = [a]

class StackClass a where
    pop :: Stack a -> (a, Stack a)
    push :: a -> Maybe (Stack a) -> Stack a 
    peek :: Stack a -> a 
    fromList :: [a] -> Stack a

instance StackClass a where
    pop ls =
        let len = length ls - 1
            last = ls !! len 
        in  (last, take len ls) 
    
    push val ls
        |isJust ls = fromMaybe [] ls ++ [val]
        |isNothing ls = [val] 

    peek ls = ls !! (length ls - 1) 

    fromList [] = []
    fromList x = x 

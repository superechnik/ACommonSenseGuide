{-
 this is the first chapter with exercises requiring us to write functions.
 in this chaper the main point is that hash tables allow us to lookup a key
 in O(1) time and so, you can avoid O(N^2) by refactoring with hash tables.
 I had to write these in awkward ways to meet the hash table requirement.
 In Haskell the closest I could find to a hash table is Data.Map and the lookup is
 actually O(log N) but the overall functions should meet the O(N) requirement.
-}
module Chapter8 
    (intersect
    ,findFirstDup
    ) where

import qualified Data.Map as Map
import Data.Maybe ( isJust )

--Excercise1
{-
Find the intersection of two lists using a hashtable in O(N)
-}
intersect :: (Ord a) => [a] -> [a] -> [a]
intersect _ [] = []
intersect list1 list2 = 
    let ht = hashTableBoolFromList list2
    in 
    [x | x <- list1, Just True ==  Map.lookup x ht]

--Excercise2
{-
Find the first ocurring duplication in an array of strings in O(N)
-}
findFirstDup :: Map.Map String Bool -> [String] -> String
findFirstDup _ [] = ""
findFirstDup ht (x:xs)  
    | alreadyExisted = x
    | otherwise = findFirstDup (snd maybeInsert) xs 
    where maybeInsert =  insertLookup x True ht
          alreadyExisted = isJust $ fst maybeInsert
          insertLookup kx x t = Map.insertLookupWithKey (\_ a _ -> a) kx x t
    
--helpers

hashTableBoolFromList :: Ord k => [k] -> Map.Map k Bool
hashTableBoolFromList x =
    Map.fromList [(ls,True) | ls <- x]
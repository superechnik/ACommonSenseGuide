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
    ,findMissingLetter
    ,firstNonDupLetter
    ) where

import qualified Data.Map as Map
import Data.Maybe ( isJust, isNothing,fromMaybe)
import Data.Char (toLower)

--trace:
--import Debug.Trace
-- firstNonDupLetter a b c| trace ("nonDups " ++ show a ++ " " ++ show c) False = undefined

--Excercise1
{-
Find the intersection of two lists using a hashtable in O(N)
-}
intersect :: (Ord a) => [a] -> [a] -> [a]
intersect _ [] = []
intersect list1 list2 = 
    let ht = hashTableFromList list2 False
    in 
    [x | x <- list1, isJust $ Map.lookup x ht]

--Excercise2
{-
Find the first ocurring duplication in an array of strings in O(N)
-}
findFirstDup :: Map.Map String Bool -> [String] -> String
findFirstDup _ [] = ""
findFirstDup ht (x:xs)  
    | alreadyExisted = x
    | otherwise = (findFirstDup . snd) maybeInsert xs 
    where maybeInsert =  insertLookup x True ht
          alreadyExisted = isJust . fst $ maybeInsert
          insertLookup kx x t = Map.insertLookupWithKey (\_ a _ -> a) kx x t

--Excercise3
{-
Given a string that contains all the letters of the alphabet except one,
return the missing letter in O(N)
-}
findMissingLetter :: String -> String 
findMissingLetter "" = "" 
findMissingLetter xs =
    let ht = hashTableFromList [toLower x | x <- xs, x /= ' '] False
    in
    [x | x <- ['a'..'z'], isNothing $ Map.lookup x ht]

--Excercise4
{-
Given a string, return the first non-duplicated letter and return it
in O(N)
-}
firstNonDupLetter m "" orig = take 1 [r | r <- orig, Map.lookup r m == Just 0]
firstNonDupLetter m (x:xs) orig = 
    let look =  Map.lookup x m
        inserted = if isJust look then Map.insert x 1 m else Map.insert x 0 m 
        newHt = Map.update f x inserted
    in  firstNonDupLetter newHt xs orig
    where
        f x = if x > 0 then Just (x+1) else Just x 
        
--helpers

hashTableFromList :: Ord k => [k] -> a -> Map.Map k a
hashTableFromList x a = 
    Map.fromList [(ls,a) | ls <- x]

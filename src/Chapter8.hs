module Chapter8 
    (intersect
    ) where

-- this chapter wants us to use hash tables
import qualified Data.Map as Map

--Excercise1

intersect :: (Ord a) => [a] -> [a] -> [a]
intersect _ [] = []
intersect list1 list2 = 
    let ht = Map.fromList [(x,True) | x <- list2] 
    in 
    [x | x <- list1, boolFromMaybe $ Map.lookup x ht]

--helpers 

boolFromMaybe :: Maybe Bool -> Bool 
boolFromMaybe a  = case a of  
    Nothing -> False
    Just _ -> True
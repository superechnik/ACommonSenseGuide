{-
This is another chapter about recursion.  All excercises must use recursion.
-}
module Chapter11 
(countChars
,justEvens
)where

--exercise 1
{-
use recursion to accept an array of strings and return the total
number of characters
-}
countChars :: [String] -> Int
countChars [] = 0 
countChars (x:xs) = 
    length x + countChars(xs)

--exercise 2
{-
use recursion to take a list of nums and return a new array
with just the evens
-}
justEvens :: (Integral a) => [a] -> [a]
justEvens [] = [] 
justEvens (x:xs)
    | even x = x : justEvens xs
    | otherwise = justEvens xs  
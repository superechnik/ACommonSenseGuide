{-
This is another chapter about recursion.  All excercises must use recursion.
-}
module Chapter11 
(countChars
)where

--excercise 1
{-
accept an array of strings and return the total number of characters
-}
countChars :: [String] -> Int
countChars [] = 0 
countChars (x:xs) = 
    length x + countChars(xs)
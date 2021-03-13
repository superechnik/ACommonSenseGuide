{-
This chapter is the first about recursion, which is so idiomatic to Haskell
that there is little point writing these.  However, excercise four requires a 
custom data struction to model the input which is interesting
-}
module Chapter10
(
readNestedList, input
)where 

import Data.Maybe

--exercise 4
{-
take this object:

-- [1,2,3,
--    [4,5,6],
--    7,
--    [8,
--      [9,10,11,
--        [12,13,14]
--          ]
--        ],
--     [15,16,17,18,19
--       [20,21,22,
--         [23,24,25,
--           [26,27,29]
--         ], 30,31
--       ], 32
--     ], 33
--    ]
and print just the numbers.

instead of "print the numbers" i will just go up to flattening the list
-}


data ListOfIntOrList = I Int | LI [ListOfIntOrList] deriving (Show)

input :: ListOfIntOrList
input = LI [I 1,I 2,I 3, LI [I 4,I 5,I 6],
   I 7,
   LI [I 8,
     LI [I 9,I 10,I 11,
       LI [I 12,I 13,I 14]
         ]
       ],
    LI [I 15,I 16,I 17,I 18,I 19,
      LI [I 20,I 21,I 22,
        LI [I 23,I 24,I 25,
          LI [I 26,I 27,I 29]
        ], I 30,I 31
      ], I 32
    ], I 33
   ]
   
readNestedList :: ListOfIntOrList -> [Int] 
readNestedList x = 
    case x of 
        LI x -> deconstructList x 
        I x ->  [x] 



--helpers        

deconstructList :: [ListOfIntOrList] -> [Int] 
deconstructList [] = []
deconstructList xs = 
  foldr (\ x -> (++) (flatten x [])) [] xs 

flatten :: ListOfIntOrList -> [Int]-> [Int]
flatten (LI []) _ = []
flatten x ls = 
    case x of 
      I x -> ls ++ [x]
      LI (x:xs) -> flatten x ls ++ flatten (LI xs) ls   
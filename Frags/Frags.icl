implementation module Frags

import StdEnv
import StdDebug

Start = frags [1,2,3,4,5]

frags					:: [a] -> [[a]]
frags [] = [[]]
frags [x:xs] =  frags1 [x:xs] (length [x:xs]-1) ++ frags xs

frags1					:: [a] Int -> [[a]]
frags1 xs n
| n <= 0 = [[hd xs]]
| otherwise = frags1 xs (n-1) ++ [xs%(0,n)]

// 1. the type is [a] -> [[a]]
// 2. frags contains n*n elements, because each value in the list has n fragments
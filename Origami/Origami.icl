module Origami

import StdEnv
import StdDebug

//	You can use this Start-function to test your functions, assuming they are called
//  sum`, prod`, flatten`, length`, reverse`, takeWhile`, maxList` (to avoid name clashes).

Start		= and
				  [
				    sum`       [1 .. 5]                 == sum       [1 .. 5]
				  , prod`      [1 .. 5]                 == prod      [1 .. 5]
				  , flatten`   [[],[1],[1,2],[1,2,3]]   == flatten   [[],[1],[1,2],[1,2,3]]
				  , length`    [1 .. 10]                == length    [1 .. 10]
				  , reverse`   [1 .. 5]                 == reverse   [1 .. 5]
				  , takeWhile` ((<>) 0) [1,2,3,0,4,5,6] == takeWhile ((<>) 0) [1,2,3,0,4,5,6]
				  , maxList`   [1 .. 5]                 == maxList   [1 .. 5]
				  ]


sum` :: [Int] -> Int
sum` x = foldr (+) 0 x

prod` :: [Int] -> Int
prod` x = foldr (*) 1 x

flatten` :: [[a]] -> [a]
flatten` x = foldr (++) [] x

length` :: [a] -> Int
length` x = foldr (\_ n = n + 1) 0 x

reverse` :: [a] -> [a]
reverse` x = foldl (\as a = [a] ++ as) [] x

takeWhile` :: (a -> Bool) [a] -> [a]
takeWhile` f l = foldr (\x xs = if (f x) [x:xs] []) [] l

maxList` :: [a] -> a | Ord a
maxList` x = foldr max (hd x) x
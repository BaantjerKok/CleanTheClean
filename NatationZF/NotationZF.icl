module NotationZF

import StdEnv
import StdDebug

// Alysha Bogaers, s4442822
// Svenja Kartoredjo, s4486811

g1 :: [a] [b] -> [(a,b)]
g1 as bs	= [(a,b) \\ a <- as, b <- bs]
// This function finds all the possible combinations of the values in a and b
// e.g. a = [1,2] and b = [3,4] -> [(1,3),(1,4),(2,3),(2,4)] (Cartesian product)

g2 :: [a] [b] -> [(a,b)]
g2 as bs = [(a,b) \\ a<-as & b<-bs]
// This function zips the two lists together
// e.g. a = [1,2] and b = [3,4] -> [(1,3),(2,4)]

g3 :: [a] [a] -> [(a,a)] | Eq a
g3 as bs	= [(a,b) \\ a <- as, b <- bs | a <> b]
// This function only finds the combinations which contain elements that are different from each other
// e.g. a = [1,2] and b = [1,2] -> [(1,2), (2,1)], because in (1,1) 1==1 and in (2,2) 2==2

g4 :: [a] [a] -> [(a,a)] | Eq a
g4 as bs = [(a, b) \\ a<-as, b<-bs | a==b]
// This function creates a list of combinations of values that appear both in a and b
// e.g. a = [1,2,3] and b = [1,2,4] -> [(1,1),(2,2)]

g5 :: [[a]] -> [a]
g5 xss = [x \\ xs <- xss, x <- xs]
// This function fuses the lists within the [[a]] to form [a]
// e.g. xss = [[1,2,3],[1,2,4]] -> [1,2,3,1,2,4]

g6 :: a [a] -> [Int] | Eq a
g6 a xs = [i \\ i <- [0..] & x <- xs | a == x]
// This function creates a list of the positions on which the value a appears in xs
// e.g. a = 3 and xs = [3,2,2,3] -> [0,3]

Start = g5 [[1,2,3],[1,2,4]]

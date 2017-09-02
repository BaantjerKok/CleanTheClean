implementation module StdSet

import StdBool, StdClass, StdFunc, StdList, StdMisc
import StdDebug

::	Set a = Set [a]

Start =  isStrictSubset (toSet [1,2,3]) (toSet [1,2,3,4,5])

toSet :: [a] -> Set a | Eq a
toSet []	= (Set [])
toSet  a	= (Set [hd a : fromSet(toSet [x \\ x <- tl a | x <> hd a ])])

fromSet :: (Set a) -> [a]
fromSet (Set []) = []
fromSet (Set a)	 = a

memberOfSet :: a (Set a) -> Bool | Eq a
memberOfSet _ (Set []) = False
memberOfSet a (Set [x:xs])
| a == x    = True
| otherwise = memberOfSet a (Set xs)

instance zero (Set a) where zero = (Set [])

isSubset :: (Set a) (Set a) -> Bool | Eq a
isSubset (Set []) (Set [])  = True
isSubset (Set [x]) y        = memberOfSet x y
isSubset (Set [x:xs]) y     = memberOfSet x y && isSubset (toSet xs) y

isStrictSubset :: (Set a) (Set a) -> Bool | Eq a
isStrictSubset x y = isSubset x y && (isSubset y x == False)

instance == (Set a) | Eq a where == x y = isSubset x y && isSubset x y

union :: (Set a) (Set a) -> Set a | Eq a
union (Set []) (Set []) = zero
union (Set []) y		= y
union  x (Set [])  		= x
union (Set x) (Set y)   = toSet [a \\ a <- x ++ y]

intersection :: (Set a) (Set a) -> Set a | Eq a
intersection (Set []) _		   = zero
intersection _ (Set [])        = zero
intersection (Set x) y = toSet [a \\ a <- x | memberOfSet a y]

without :: (Set a) (Set a) -> Set a | Eq a
without (Set []) _ = zero
without x (Set []) = x
without (Set x) y  = toSet [a \\ a <- x | (memberOfSet a y) == False]

powerSet :: (Set a) -> Set (Set a)
powerSet (Set x) = Set [Set [a,b] \\ a <- x, b <- x]

isEmptySet :: (Set a) -> Bool
isEmptySet (Set []) = True
isEmptySet (Set x)	= False

isDisjoint :: (Set a) (Set a) -> Bool | Eq a
isDisjoint (Set []) _ = True
isDisjoint (Set [x:xs]) y
| memberOfSet x y	  = False
| otherwise 		  = isDisjoint (Set xs) y

product :: (Set a) (Set b) -> Set (a,b)
product (Set x) (Set y) = Set [(a,b) \\ a <- x, b <- y]

numberOfElements :: (Set a) -> Int
numberOfElements (Set []) = 0
numberOfElements (Set a)  = length a



Given:

(++) :: [a] [a] -> [a]
(++) []     xs = xs                (1)
(++) [y:ys] xs = [y : ys ++ xs]    (2)

map :: (a -> b) [a] -> [b]
map f []       = []                (3)
map f [x:xs]   = [f x : map f xs]  (4)

flatten :: [[a]] -> [a]
flatten []     = []                (5)
flatten [x:xs] = x ++ (flatten xs) (6)

1. 
Prove:
	For every function f, finite list as and bs:
		
		map f (as ++ bs) = (map f as) ++ (map f bs)
		
Proof by induction on as
Base step: assume as = []

		map f (as ++ bs)				// assumption 		as = []
	
	=	map f ([] ++ bs)				// def of (++), 	rule (1)
			  ^^^^^^^^^
	=	map f (bs)						// def of (++), 	rule (1) <=
			  ^^^^	
	=	[] ++ map f bs					// def of map, 		rule (3) <=
		   ^^
	=	(map f []) ++ (map f bs)		// assumption 		as = []  <=
		       ^^
	= 	(map f as) ++ (map f bs)		
	
	So for as = []: map f ([] ++ ba) = (map f as) ++ (map f bs)

Induction:
	assume: as 
	map f (as ++ bs) = (map f as) ++ (map f bs)		// (IH)
	
	Prove: [a : as]
	map f ([a:as] ++ bs) = (map f [a:as]) ++ (map f bs)

		map f ([a : as] ++ bs)			// def of (++), rule (2)
		       ^^^^^^^^^^^^^^
	= 	map f [a : as ++ bs]			// def of map, 	rule(4)
					  ^^
	= 	[f a : map f (as ++ bs)]		// IH
		^^^^^^^^^^^^^^^^^^^^^^^^
	=	[f a : (map f as) ++ (map f bs)]// def of (++), rule (2) <=
		       ^^^^^^^^^^^^^^^^^^^^^^^^
	= 	[f a : map f as] ++ (map f bs) 	// def of map, 	rule (4) <=
						 ^^
	= 	(map f [a : as]) ++ (map f bs)	
		

	basic step, induction step: as = [a : as]: map f (as ++ bs) = (map f as) ++ (map f bs)


2. 
Prove:
	for every function f, for every finite list xs:
	
		flatten (map (map f) xs) = map f (flatten xs)
		
Proof by induction on xs
Base step: assume xs = []

		flatten (map (map f) xs)		// assumption 		xs = []
	
	=	flatten (map (map f) [])		// def of map, 		rule (3)
		         ^^^^^^^^^^^^^^
	=	flatten []						// def of flatten, 	rule (5)
		^^^^^^^^^^
	= 	[]								// def of map, 		rule (3) <=
		^^
	=	map f [] 						// def of flatten, 	rule (5) <=
			  ^^
	= 	map f (flatten [])				// assumption 		xs = []  <=
					   ^^	
	= 	map f (flatten xs)				

Induction:
	So for xs = []: flatten (map (map f) xs) = map f (flatten xs) 	// (IH)
		
	prove: [x : xs]	
	flatten (map (map f) [x : xs]) = map f (flatten [x : xs])
		
		flatten (map (map f) [x : xs])			// def of map, 		rule (4)
				 ^^^^^^^^^^^^^^^^^^^^
	=	flatten [(map f) x : map (map f) xs]	// def of flatten, 	rule (6)
	    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
	=	(map f x) ++ flatten (map (map f) xs)	// IH
					 ^^^^^^^^^^^^^^^^^^^^^^^^
	=	(map f x) ++ map f (flatten xs)			// 9.4.1 <=
	    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
	=	map f (x ++ (flatten xs))				// def of flatten, 	rule(6) <= 
			   ^^^^^^^^^^^^^^^^	
	=	map f (flatten [x : xs])				

	basic step, induction step: xs = [x : xs]: flatten (map (map f) xs) = map f (flatten xs)


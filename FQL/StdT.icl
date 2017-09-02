implementation module StdT

import StdEnv

::	T = {m :: Int, s :: Int} 

instance ==		T where == a b = a.m == b.m && a.s == b.s
instance <		T where < a b = a.m < b.m || a.m == b.m && a.s < b.s

instance zero		T where zero = {m = zero, s = zero}
instance +		T where + a b = fromInt (toInt a + toInt b) 
instance -		T where - a b = if (a < b) zero (fromInt (toInt a - toInt b))

instance toInt		T where toInt a = a.m * 60 + a.s
instance fromInt	T where fromInt	n = if (n < 0) zero {m = n/60, s = n rem 60}

instance toString	T where 
	toString {m = x, s = 0} = toString x +++ ":00"
	toString a = toString a.m +++ ":" +++ (if (a.s < 10) "0" "") +++ toString a.s
instance fromString	T where 
	fromString s = if (s.[size s - 3] == ':') 
		{m = toInt (s % (0, size s - 4)), s = toInt (s % (size s - 2, size s - 1))} 
		zero

Start :: (Bool, Bool, T, T, T, Int, String, T, T)
Start = (LOTR == Tea, Tea < LOTR, 
	zero + LOTR, LOTR + Tea, Tea - LOTR, 
	toInt LOTR, toString Tea, 
	fromString "5:40", fromString "foo")

LOTR = {m=178, s=0}
Tea = {m=0,s=41}
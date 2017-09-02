implementation module StdT

import StdEnv
import StdDebug

::	T  = {min :: Int , sec :: Int } // define your implementation of T here: 

Start = {min=6, sec=32} + {min=5, sec=40}

instance ==			T where == x y = x.min == y.min && x.sec == y.sec

instance <			T where < x y = x.min < y.min || x.min == y.min && x.sec < y.sec

instance zero		T where zero = {min = 0, sec = 0}

instance +			T where + x y = fromInt (toInt x + toInt y)

instance -			T where - x y  = if (x < y) zero (fromInt (toInt x - toInt y))

instance toInt		T where toInt x = x.min*60 + x.sec

instance fromInt	T where fromInt x = if (x<0) zero {min = x/60, sec = x rem 60}

instance toString	T where toString x = toString x.min +++ ":" +++ toString x.sec

instance fromString	T where fromString x =  if (x.[size x] == ':') {min = toInt(x%(0, size x-4)), sec = toInt (x%(size x-2, size x-1))} zero
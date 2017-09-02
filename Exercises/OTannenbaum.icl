implementation module OTannenbaum

import StdEnv
import StdDebug

Start					= triangle 5 0
//Start					= christmastree 5

triangle				:: Int Int -> String
triangle n m
| n == 0 	= ""
| otherwise = spaces n +++ stars m +++ '\n' +++ triangle (n-1) (m+1)

stars					:: Int -> String
stars n
| n <= 0 	= ""
| otherwise = toString '*' +++ stars (n-1) 

spaces					:: Int -> String
spaces n
| n == 0    = ""
| otherwise = " " +++ spaces (n-1)

christmastree				:: Int -> String
christmastree _ = trace_n "christmastree not yet implemented" ""


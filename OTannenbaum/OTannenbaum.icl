implementation module OTannenbaum

import StdEnv
import StdDebug

//Start					= "\n" +++ triangle 5 0
Start					= "\n" +++ christmastree 5 0

triangle				:: Int Int -> String
triangle 0 m = ""
triangle n m = space n +++ stars (m+1) +++ toString '\n' +++ triangle (n-1) (m+2)

stars					:: Int -> String
stars n
| n <= 0 = ""
| otherwise = "*" +++ stars (n-1) 

space					:: Int -> String
space 0 = ""
space n = " " +++ space (n-1)

christmastree				:: Int Int -> String
christmastree n 0 = space n +++ "*\n" +++ triangle n 0 +++ christmastree (n+1) 1
christmastree n 3 = ""
christmastree n m = triangle n 0 +++ christmastree (n+1) (m+1)


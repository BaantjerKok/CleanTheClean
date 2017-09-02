implementation module MatchStrings

import StdEnv  
import StdDebug

head				:: String -> Char
head s = s.[0]

tail				:: String -> String
tail s = s%(1,size(s)-1)

is_equal			:: String String -> Bool
is_equal s1 s2
| size s1 <> size s2 = False
| size s1 == 0 && size s2 == 0 = True
| head s1 <> head s2 = False
| otherwise = is_equal (tail s1) (tail s2)

is_substring		:: String String -> Bool
is_substring s1 s2
| s1 == s2%(1, size(s1)) = True
| size s2 < size s1 = False
| otherwise = is_substring s1 (tail s2)

is_sub				:: String String -> Bool
is_sub s1 s2
| size s2 < size s1  = False
| size s1 == 0       = True
| head s1 == head s2 = is_sub (tail s1) (tail s2)
| head s1 <> head s2 = is_sub s1 (tail s2)
| otherwise          = False

is_match			:: String String -> Bool
is_match p s
| size s < size p  = False
| size p == 0      = True
| head p <> head s || head p == '.'	= is_match (tail p) (tail s)
| (head (tail p) == '*') && (head p == head s) 	= is_match p (tail s)
| otherwise				= False

Start
// ad-hoc tests:
//					= (head pink_floyd, tail pink_floyd)
//					= is_equal "  1" "  1"
//					= is_substring "hello" "hello"
//					= is_substring "there" pink_floyd
//					= is_substring "there" marillion
//					= is_sub "there" marillion
//					= is_sub "she and her" pink_floyd
//					= is_sub radiohead pink_floyd
//					= is_match "*.here*.here*." pink_floyd
					= is_match ".here.here." pink_floyd

pink_floyd			= "Is there anybody in there?"
marillion			= "Just for the record"
radiohead			= "There there"

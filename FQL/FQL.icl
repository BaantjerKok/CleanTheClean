module FQL

import StdEnv
import StdDebug
import StdT

//Our time implementation did not work fully,
//When testing a function that should return time, it would return 'T', 
//but we where unsure if that was because of the implementatins here, or in the old time assignment

::  Song = { group   :: String         // Name of the group
           , album   :: String         // Name of the album
           , year    :: Int            // Release date
           , track   :: Int            // Track nr. (1..)
           , title   :: String         // Title of the song
           , tags    :: [String]       // Descriptive tags of the album / song
           , time    :: T              // Playing time of the song
           , country :: [String]       // Country of origin of the group
           }

Start world
# (ok,dbs,world)        = fopen "FQL-songs.dbs" FReadText world
| not ok                = abort "Couldn't open 'FQL-songs.dbs'."
# (inhoud,dbs)          = filelines dbs
# (ok,world)            = fclose dbs world
| not ok                = abort "Couldn't close 'FQL-songs.dbs' after reading."
# nummersDB             = [ { group   = group
                            , album   = cd
                            , year    = toInt year
                            , track   = toInt track
                            , title   = title
                            , tags    = sort (symbol_separated_list ',' tags)
                            , time    = fromString length
                            , country = sort (symbol_separated_list ',' countries)
                            }
                         \\ [_,group,cd,year,track,title,tags,length,countries]
                            <- collect (nr_of_fields+1)   // collect all elements of an entry
                               (drop (nr_of_fields+1)     // delete the headers
                               (map initString inhoud))   // remove the \n
                          ]
= (all_albums_of "Adele" nummersDB,world)
where
	nr_of_fields = 8

all_groups              :: [Song] -> [String]
all_groups songs = groups songs

groups 					:: [Song] -> [String]
groups songs = removeDup [ group \\ {group} <- songs ]

//--------------------------------

all_periods             :: [Song] -> [String]
all_periods songs = notation (sort (removeDup [ year \\ {year} <- songs ]))

notation :: [Int] -> [String]
notation [x:xs] = notation2 xs x 1

notation2 :: [Int] Int Int -> [String]
notation2 [] _ _ = []
notation2 [x] jaar _ = [toString jaar +++ "-" +++ toString x]
notation2 [x:xs] jaar plus
| (jaar+plus) == x		= notation2 xs jaar (plus+1)
| jaar == (jaar+plus-1) = [toString jaar] ++ notation2 xs x 1
| otherwise 			= [toString jaar +++ "-" +++ toString (jaar+plus-1)] ++ notation2 xs x 1

//--------------------------------

all_albums_of           :: String [Song] -> [(Int,String)]
all_albums_of group songs = orderAlbums (getYearAlbums (getGroups group songs))

getGroups 				:: String [Song] -> [Song]
getGroups group_name songs = [s \\ s <- songs | s.group == group_name]

getYearAlbums 				:: [Song] -> [(Int,String)]
getYearAlbums songs = [(year, album) \\ {year} <- songs & {album} <- songs]

orderAlbums 			:: [(Int,String)] -> [(Int,String)]
orderAlbums albums = sort (removeDup albums)

//--------------------------------

all_tracks              :: String String [Song] -> [(Int,String,T)]
all_tracks album group songs = orderTracks (getTracks ( getAlbums album (getGroups group songs)))

getAlbums 				:: String [Song] -> [Song]
getAlbums album_name songs = [ s \\ s <- songs | s.album == album_name]

getTracks 				:: [Song] -> [(Int,String,T)]
getTracks songs = [(track,title,time) \\ {track} <- songs & {title} <- songs & {time} <- songs]

orderTracks 			:: [(Int,String,T)] -> [(Int,String,T)]
orderTracks tracks = sort (removeDup tracks)

//--------------------------------

time_albums             :: [Song] -> [(T,String,String)]
time_albums songs = cumulativeTime (getTGA songs)

getTGA :: [Song] ->  [(T,String,String)]
getTGA songs = [(time,group,album) \\ {time} <- songs & {group} <- songs & {album} <- songs]
 
cumulativeTime ::  [(T,String,String)] -> [(T,String,String)]
cumulativeTime [] = []
cumulativeTime [x:xs] = cumulativeTime2 x (hd xs) (tl xs)

cumulativeTime2 ::  (T,String,String) (T,String,String) [(T,String,String)] -> [(T,String,String)]
cumulativeTime2 _ _ [] = []
cumulativeTime2 (t,g,a) (ts,gs,as) [x:xs]
| g==gs && a==as	= 	cumulativeTime2 (t+ts,g,a) x xs
| otherwise			=	[(t,g,a)] ++ cumulativeTime2 (ts,gs,as) x xs

//--------------------------------

total_time              :: [Song] -> T
total_time songs =  sum ([time \\ {time} <- songs])

//--------------------------------

dutch_metal             :: [Song] -> [String]
dutch_metal songs = removeDup [toString (group) \\ {group} <- (getDutchMetals songs)]

getDutchMetals :: [Song] -> [Song]
getDutchMetals songs = [s \\ s <- songs | isElem "metal" s.tags && isElem "Netherlands" s.country]

isElem:: a [a] -> Bool | Eq, Ord a
isElem e [] = False
isElem e [x:xs]= e == x || (e > x && isElem e xs)

//--------------------------------

/* The functions below are needed in the Start rule to read the file 'FQL-songs.dbs'.
 * You do not need to understand how they work.
 */

//  filelines reads all lines in a File
filelines               :: !*File -> (![String],!*File)
filelines file
# (end,file)            = fend file
| end                   = ([],file)
# (line,file)           = freadline file
# (lines,file)          = filelines file
= ([line:lines],file)

//  initString removes whitespace at the end of a String
initString              :: (String -> String)
initString				= toString o reverse o (dropWhile isSpace) o reverse o fromString

/*  collect n [x_1, ..., x_n, x_{n+1}, ... x_{2n}, ..., x_{mn+1} ... x_{mn+k}]
        = [[x_1, ..., x_n], [x_{n+1}, ... x_{2n}], ..., [x_{(m-1)n+1} ... x_{mn}]]
    where:
        n > 0 /\ m >= 0 /\ k <= n
*/
collect                 :: !Int ![x] -> [[x]]
collect n list
| length groupN < n     = []
| otherwise             = [groupN:collect n list`]
where
    (groupN,list`)      = splitAt n list

symbol_separated_list   :: !Char !String -> [String]
symbol_separated_list c str
                        = filter (\str -> str <> "" && str <> (toString c)) [toString cs \\ cs <- group` ((==) c) (fromString str)]
where
//	eliminates unnecessary dependency on other assignments, so define as local function
	group`				:: (a -> Bool) [a] -> [[a]]
	group` p []			= []
	group` p xs			= [yes,no:group` p more]
	where
		(yes,no_more)	= span p xs
		(no,more)		= span (not o p) no_more

implementation module Card

import StdEnv
import StdDebug

//	1.
// Define Card, Suit, Value here
:: Card = {suit :: Suit, value :: Value}
:: Suit = Heart | Diamond | Spade | Club | Non
:: Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace | None

Start = 
	// test1
	// test2
	// test3
	   test4
	   

test1 :: Bool
test1 = {suit=Spade, value=Six} == {suit=Spade, value=Seven}

test2 :: Bool
test2 = {suit=Spade, value=Six} == {suit=Spade, value=Six}

test3 :: String
test3 = toString {suit=Diamond, value=Queen}

test4 :: Card
test4 = fromString "Heart Jack"

//	2.
instance == Card where
	== x y = x.suit == y.suit && x.value == y.value

instance == Suit where
	== Heart Heart = True
	== Diamond Diamond = True
	== Spade Spade = True
	== Club Club = True
	== _ _ = False
	
instance == Value where
	== Two Two = True
	== Three Three = True
	== Four Four = True
	== Five Five = True
	== Six Six = True
	== Seven Seven = True
	== Eight Eight = True
	== Nine Nine = True
	== Ten Ten = True
	== Jack Jack = True
	== Queen Queen = True
	== King King = True
	== Ace Ace = True
	== _ _ = False
	
//	3.
instance toString Card where
	toString x = toString x.suit +++ " " +++ toString x.value

instance toString Suit where
	toString Heart   = "Heart"
	toString Diamond = "Diamond"
	toString Spade   = "Spade"
	toString Club    = "Club"
	toString Non    = ""

instance toString Value where
	toString Two   =  "Two"
	toString Three = "Three"
	toString Four  = "Four"
	toString Five  = "Five"
	toString Six   = "Six"
	toString Seven = "Seven"
	toString Eight = "Eight"
	toString Nine  = "Nine"
	toString Ten   = "Ten"
	toString Jack  = "Jack"
	toString Queen = "Queen"
	toString King  = "King"
	toString Ace   = "Ace"
	toString None  = ""
	
instance fromString Card where
	fromString x = {suit = fromString (toString (takeWhile ((<>) ' ') (fromString (toString x)))), value =  fromString (toString (dropWhile ((<>) ' ') (fromString (toString x))))}

instance fromString Suit where
	fromString "Heart" = Heart
	fromString "Diamond" = Diamond
	fromString "Spade" = Spade
	fromString "Club" = Club
	fromString _   = Non


instance fromString Value where
	fromString " Two" =  Two
	fromString " Three" = Three
	fromString " Four" = Four
	fromString " Five" = Five
	fromString " Six" = Six
	fromString " Seven" = Seven
	fromString " Eight" = Eight
	fromString " Nine" = Nine
	fromString " Ten" = Ten
	fromString " Jack" = Jack
	fromString " Queen" = Queen
	fromString " King" = King
	fromString " Ace" = Ace
	fromString _ = None
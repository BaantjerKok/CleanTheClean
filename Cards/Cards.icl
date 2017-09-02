implementation module Cards

import StdEnv
import StdDebug

// Correctly ordered deck
suits = [Heart, Diamond, Spade, Club]
values = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]

// Unordered test deck
suitsT = [Heart, Club, Spade, Diamond]
valuesT = [Two, Four, Five, Six, Seven, Three, Eight, Nine, Ten, Jack, Queen, King, Ace]

carddeck								:: [Card]
carddeck = [{suit=s, value=v} \\ s <- suitsT, v <- valuesT]

sort_by_value							:: [Card] -> [Card]
sort_by_value c = [{suit=s, value=v} \\  v <- values, s <- suits | inDeck {suit=s, value=v} c]

sort_by_suit							:: [Card] -> [Card]
sort_by_suit c = [{suit=s, value=v} \\   s <- suits, v <- values | inDeck {suit=s, value=v} c]

// Function to check if the generated card is in the original deck.
inDeck:: a [a] -> Bool | Eq a
inDeck c [] = False
inDeck c [x:xs]= c == x || inDeck c xs

Start = sort_by_value carddeck

// -----------------------------------------------------------------------------------------
// For some reason the import didn't work for us, therefore we included everything from our
// card implementation in this icl-file in order to run the code.

//	1.
// Define Card, Suit, Value here
:: Card = {suit :: Suit, value :: Value}
:: Suit = Heart | Diamond | Spade | Club | Non
:: Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace | None


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
definition module Cards

import StdEnv

carddeck      :: [Card]				// complete set of cards
sort_by_value :: [Card] -> [Card]	// sort a deck of cards, first by value, then by suit
sort_by_suit  :: [Card] -> [Card]	// sort a deck of cards, first by suit, then by value
:: Card
:: Suit
:: Value

// Copy your type definitions of Card, Suit, Value here to make them available for other modules:

instance ==         Card

instance ==			Suit

instance ==			Value

instance toString   Card

instance toString	Suit

instance toString	Value

instance fromString Card

instance fromString Suit

instance fromString Value
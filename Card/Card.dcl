definition module Card

import StdEnv

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
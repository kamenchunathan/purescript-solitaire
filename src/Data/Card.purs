module Data.Card
  ( Suit(..)
  , Card(..)
  , CardColour(..)
  , Value(..)
  , NormalCard
  , color
  ) where

import Prelude

import Data.Int (decimal, toStringAs)

data Suit
  = Spades
  | Hearts
  | Clubs
  | Diamonds

instance showSuit :: Show Suit where
  show Spades = "Spades"
  show Hearts = "Hearts"
  show Clubs = "Clubs"
  show Diamonds = "Diamonds"

instance eqSuit :: Eq Suit where
  eq Spades Spades = true
  eq Hearts Hearts = true
  eq Clubs Clubs = true
  eq Diamonds Diamonds = true
  eq _ _ = false

data CardColour
  = Black
  | Red

instance showCardColour :: Show CardColour where
  show Black = "Black"
  show Red = "Red"

instance eqCardColour :: Eq CardColour where
  eq Red Red = true
  eq Black Black = true
  eq _ _ = false

data Value
  = Ace
  | Num Int
  | Jack
  | Queen
  | King

instance showCardValue :: Show Value where
  show Ace = "Ace"
  show (Num i) = toStringAs decimal i
  show Jack = "Jack"
  show Queen = "Queen"
  show King = "King"

instance eqCardValue :: Eq Value where
  eq Ace Ace = true
  eq (Num i) (Num j) = i == j
  eq Jack Jack = true
  eq Queen Queen = true
  eq King King = true
  eq _ _ = false

type NormalCard =
  { value :: Value
  , suit :: Suit
  }

-- The higher kind allows additional information to be stored to the card type that
-- is relevant to the card but not core to the type
-- for example if a card is facing up or down
--/ Card { flipped :: False }
data Card a
  = NormalCard NormalCard a
  | Joker CardColour a

instance Show a => Show (Card a) where
  show (NormalCard c _) = show c
  show (Joker Red _) = "Red joker"
  show (Joker Black _) = "Black joker"

instance Eq a => Eq (Card a) where
  eq (Joker col1 a1) (Joker col2 a2) = col1 == col2 && a1 == a2
  eq (NormalCard nc1 a1) (NormalCard nc2 a2) = nc1 == nc2 && a1 == a2
  eq _ _ = false

instance Functor Card where
  map f (NormalCard info a) = NormalCard info (f a)
  map f (Joker info a) = Joker info (f a)

color :: forall a. (Card a) -> CardColour
color (NormalCard c _) =
  case c.suit of
    Spades -> Black
    Clubs -> Black
    _ -> Red
color (Joker cardColor _) = cardColor


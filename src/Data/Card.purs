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

instance Show Suit where
  show Spades = "Spades"
  show Hearts = "Hearts"
  show Clubs = "Clubs"
  show Diamonds = "Diamonds"

instance Eq Suit where
  eq Spades Spades = true
  eq Hearts Hearts = true
  eq Clubs Clubs = true
  eq Diamonds Diamonds = true
  eq _ _ = false

data CardColour
  = Black
  | Red

instance Show CardColour where
  show Black = "Black"
  show Red = "Red"

instance Eq CardColour where
  eq Red Red = true
  eq Black Black = true
  eq _ _ = false

data Value
  = Ace
  | Num Int
  | Jack
  | Queen
  | King

instance Show Value where
  show Ace = "Ace"
  show (Num i) = toStringAs decimal i
  show Jack = "Jack"
  show Queen = "Queen"
  show King = "King"

instance Eq Value where
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

data Card
  = NormalCard NormalCard
  | Joker CardColour

instance Show Card where
  show (NormalCard c) = show c
  show (Joker Red) = "Red joker"
  show (Joker Black) = "Black joker"

instance Eq Card where
  eq (Joker col1) (Joker col2) = col1 == col2
  eq (NormalCard nc1) (NormalCard nc2) = nc1 == nc2
  eq _ _ = false

color :: Card -> CardColour
color (NormalCard c) =
  case c.suit of
    Spades -> Black
    Clubs -> Black
    _ -> Red
color (Joker cardColor) = cardColor


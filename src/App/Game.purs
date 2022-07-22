module App.Game where

import Prelude
import Data.Array (reverse)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


data Suit
    = Spades
    | Hearts
    | Clubs
    | Diamonds

data Colour
    = Black
    | Red

data Value 
    = Ace
    | Num Int
    | Jack
    | Queen
    | King


type NormalCard = 
    { value :: Value
    , suit :: Suit
    }


data Card 
    = Card NormalCard 
    | Joker Colour


type Pile = Array Card


type State = 
    { stock         :: Pile
    -- 7 Piles with only the top card face up
    , tableau       :: Array Pile        
     -- a pile where cards are dealt from the stock 
    , waste         :: Pile            
     -- 4 stacks of cards, one for each suit in ascending order
    , foundations   :: Array Pile       
    } 


data Action
    = NoOp


orderedDeck :: Pile 
orderedDeck = 
    [ Joker Black
    , Joker Red
    ] 
    <> singleSuit Spades
    <> singleSuit Diamonds
    <> ( reverse $ singleSuit Clubs  )
    <> ( reverse $ singleSuit Hearts )


singleSuit :: Suit -> Pile 
singleSuit suit = 
    [ Card { value: Ace,    suit : suit }
    , Card { value: Num 2,  suit : suit }
    , Card { value: Num 3,  suit : suit }
    , Card { value: Num 4,  suit : suit }
    , Card { value: Num 5,  suit : suit }
    , Card { value: Num 6,  suit : suit }
    , Card { value: Num 7,  suit : suit }
    , Card { value: Num 8,  suit : suit }
    , Card { value: Num 9,  suit : suit }
    , Card { value: Num 10, suit : suit }
    , Card { value: Jack,   suit : suit }
    , Card { value: Queen,  suit : suit }
    , Card { value: King,   suit : suit }
    ]


component :: forall q i o m. H.Component q i o m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }


initialState :: forall input. input -> State
initialState _ = 
  { stock         : [] 
  , tableau       : []   
  , waste         : []   
  , foundations   : []
  }                


render :: forall cs m. State -> H.ComponentHTML Action cs m
render _ =
    HH.div 
    [] 
    [ HH.img [ HP.src "./assets/joker.png" ]
    ]

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
    _ -> H.modify_ initialState

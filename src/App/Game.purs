module App.Game where

import Prelude
import Data.Array (reverse, range, splitAt, (:))
import Data.Int (toStringAs, decimal)
import Data.Foldable (foldr)
import Data.String (toLower, joinWith)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


------------------------------------------------ MODEL -------------------------------------------------

data Suit
    = Spades
    | Hearts
    | Clubs
    | Diamonds

instance showSuit :: Show Suit where
    show Spades     = "Spades"
    show Hearts     = "Hearts"
    show Clubs      = "Clubs"
    show Diamonds   = "Diamonds"


data CardColour
    = Black
    | Red

instance showCardColour :: Show CardColour where
    show Black  = "Black"
    show Red    = "Red"


data Value 
    = Ace
    | Num Int
    | Jack
    | Queen
    | King

instance showCardValue :: Show Value where
    show Ace        = "Ace"
    show ( Num i )  = toStringAs decimal i
    show Jack       = "Jack"
    show Queen      = "Queen"
    show King       = "King"



type NormalCard = 
    { value :: Value
    , suit :: Suit
    }
 

data Card 
    = NormalCard NormalCard 
    | Joker CardColour

instance showCard :: Show Card where
    show ( NormalCard c ) = show c
    show ( Joker Red    ) = "Red joker"
    show ( Joker Black  ) = "Black joker"


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


initialState :: forall input. input -> State
initialState _ =
    { stock
    , tableau
    , waste         : []
    , foundations   : []
    }
    where 
        { tableau, stock } = splitDecktoTableauAndStock orderedDeck
    



orderedDeck :: Pile
orderedDeck =
    [ Joker Black , Joker Red ]
    <> singleSuit Spades
    <> singleSuit Diamonds
    <> ( reverse $ singleSuit Clubs  )
    <> ( reverse $ singleSuit Hearts )


singleSuit :: Suit -> Pile 
singleSuit suit = 
    [ NormalCard { value: Ace,    suit : suit }
    , NormalCard { value: Num 2,  suit : suit }
    , NormalCard { value: Num 3,  suit : suit }
    , NormalCard { value: Num 4,  suit : suit }
    , NormalCard { value: Num 5,  suit : suit }
    , NormalCard { value: Num 6,  suit : suit }
    , NormalCard { value: Num 7,  suit : suit }
    , NormalCard { value: Num 8,  suit : suit }
    , NormalCard { value: Num 9,  suit : suit }
    , NormalCard { value: Num 10, suit : suit }
    , NormalCard { value: Jack,   suit : suit }
    , NormalCard { value: Queen,  suit : suit }
    , NormalCard { value: King,   suit : suit }
    ]


splitDecktoTableauAndStock :: Pile -> {tableau :: Array Pile, stock :: Pile }
splitDecktoTableauAndStock deck =
    { tableau   : res.before
    , stock     : res.after
    }
    where
        res = foldr 
            (\i tab ->  { after:  (splitAt i tab.after).after
                        , before: (splitAt i tab.after).before : tab.before 
                        }
                    )
            { after: deck, before: [] }
            ( range 1 7 )


color :: Card -> CardColour
color ( NormalCard c ) = 
    case c.suit of 
        Spades -> Black
        Clubs -> Black
        _ -> Red
color ( Joker cardColor ) = cardColor


cardImageUri :: Card -> String
cardImageUri ( NormalCard c ) =
    ( toLower $ joinWith "_" [ show c.value, show c.suit ] ) <> ".png"
cardImageUri ( Joker _ ) =  
    "joker.png"
    -- ( toLower $ joinWith "_" ["joker", show col ]) <> ".png"

------------------------------------------------ UPDATE -------------------------------------------------


data Action
    = NoOp


handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
    _ -> H.modify_ initialState


------------------------------------------------ RENDER -------------------------------------------------

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
    HH.div
    []
    ( map (\card -> HH.img [ HP.src $ "./assets/" <> ( cardImageUri card) ] ) state.stock )


component :: forall q i o m. H.Component q i o m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }



module App.Game where

import Prelude

import Data.Array (head, replicate, reverse, splitAt, tail, (..), (:))
import Data.Foldable (foldr)
import Data.Int (toStringAs, decimal)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toLower, joinWith)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.HTML.Event.DragEvent (DragEvent, toEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

------------------------------------------------ MODEL -------------------------------------------------

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

data CardColour
  = Black
  | Red

instance showCardColour :: Show CardColour where
  show Black = "Black"
  show Red = "Red"

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

type NormalCard =
  { value :: Value
  , suit :: Suit
  }

data Card
  = NormalCard NormalCard
  | Joker CardColour

instance showCard :: Show Card where
  show (NormalCard c) = show c
  show (Joker Red) = "Red joker"
  show (Joker Black) = "Black joker"

type Pile = Array Card

type State =
  { stock :: Pile
  -- 7 Piles with only the top card face up
  , tableau :: Array (Maybe Pile)
  -- a pile where cards are dealt from the stock 
  , waste :: Pile
  -- 4 stacks of cards, one for each suit in ascending order
  , foundations :: Array (Maybe Pile)
  }

initialState :: forall input. input -> State
initialState _ =
  { stock
  , tableau
  , waste: []
  , foundations: replicate 4 Nothing
  }
  where
  { tableau, stock } = splitDecktoTableauAndStock orderedDeck

orderedDeck :: Pile
orderedDeck =
  [ Joker Black, Joker Red ]
    <> singleSuit Spades
    <> singleSuit Diamonds
    <> (reverse $ singleSuit Clubs)
    <> (reverse $ singleSuit Hearts)

singleSuit :: Suit -> Pile
singleSuit suit =
  [ NormalCard { value: Ace, suit: suit }
  , NormalCard { value: Num 2, suit: suit }
  , NormalCard { value: Num 3, suit: suit }
  , NormalCard { value: Num 4, suit: suit }
  , NormalCard { value: Num 5, suit: suit }
  , NormalCard { value: Num 6, suit: suit }
  , NormalCard { value: Num 7, suit: suit }
  , NormalCard { value: Num 8, suit: suit }
  , NormalCard { value: Num 9, suit: suit }
  , NormalCard { value: Num 10, suit: suit }
  , NormalCard { value: Jack, suit: suit }
  , NormalCard { value: Queen, suit: suit }
  , NormalCard { value: King, suit: suit }
  ]

splitDecktoTableauAndStock :: Pile -> { tableau :: Array (Maybe Pile), stock :: Pile }
splitDecktoTableauAndStock deck =
  { tableau: map Just res.before
  , stock: res.after
  }
  where
  res = foldr
    ( \i tab ->
        { after: (splitAt i tab.after).after
        , before: (splitAt i tab.after).before : tab.before
        }
    )
    { after: deck, before: [] }
    (1 .. 7)

color :: Card -> CardColour
color (NormalCard c) =
  case c.suit of
    Spades -> Black
    Clubs -> Black
    _ -> Red
color (Joker cardColor) = cardColor

------------------------------------------------ UPDATE -------------------------------------------------

data Action
  = NoOp
  | DragStart DragEvent
  | DragEnter DragEvent
  | DragOver DragEvent
  | DropCard DragEvent
  | DealFromStock MouseEvent

handleAction :: forall output m. MonadEffect m => Action → H.HalogenM State Action () output m Unit
handleAction = case _ of
  DragStart _ ->
    H.liftEffect do
      --TODO: temporary effect which should be replaced with behaviour expecteded on dragging over the pile
      H.liftEffect $ log "drag start"

  DragOver e -> H.liftEffect $ preventDefault $ toEvent e

  DragEnter e -> H.liftEffect do
    preventDefault $ toEvent e
    log "drag enter"

  DropCard _ -> H.liftEffect $ log "dropping"

  DealFromStock _ -> do
    H.modify_
      ( \s ->
          case s.stock of
            [] -> s
              { stock = reverse s.waste
              , waste = []
              }
            _ -> s
              { stock = fromMaybe [] $ tail s.stock
              , waste = fromMaybe [] $ map (\m -> m : s.waste) (head s.stock)
              }
      )

  NoOp -> pure unit

------------------------------------------------ RENDER -------------------------------------------------

cardImageUri :: Card -> String
cardImageUri (NormalCard c) =
  (toLower $ joinWith "_" [ show c.value, show c.suit, "white" ]) <> ".png"
cardImageUri (Joker _) =
  "joker_white.png"

-- ( toLower $ joinWith "_" ["joker", show col ]) <> ".png"

backCardFace :: String
backCardFace = "back_blue_basic.png"

emptySlot :: forall cs m. H.ComponentHTML Action cs m
emptySlot =
  HH.div [ HP.class_ $ HH.ClassName "empty-slot" ] []

renderStock :: forall cs m. Pile -> H.ComponentHTML Action cs m
renderStock [] =
  HH.div
    [ HP.class_ $ HH.ClassName "slot stock"
    , HE.onClick DealFromStock
    ]
    [ emptySlot ]

renderStock _ =
  HH.div
    [ HP.class_ $ HH.ClassName "slot stock"
    , HP.draggable false
    , HE.onClick DealFromStock
    ]
    [ HH.img
        [ HP.draggable false
        , HP.src $ "./assets/" <> (backCardFace)
        ]
    ]

waste :: forall cs m. Pile -> H.ComponentHTML Action cs m
waste wastePile =
  HH.div
    [ HP.class_ $ HH.ClassName "slot waste" ]
    [ fromMaybe emptySlot ((\topCard -> HH.img [ HP.src $ "./assets/" <> (cardImageUri $ topCard) ]) <$> (head wastePile))
    ]

foundations :: forall cs m. Array (Maybe Pile) -> H.ComponentHTML Action cs m
foundations fPiles =
  HH.div
    [ HP.class_ $ HH.ClassName "foundations slot" ]
    ( map
        ( \maybePile ->
          -- TODO fix: the empty slot should only be a graphic and should not take away functionaltiy
          -- of the pile as it does now
            fromMaybe emptySlot
              ( ( \pile ->
                    HH.div
                      [ HE.onDragEnter DragEnter
                      , HE.onDragOver DragOver
                      , HE.onDrop DropCard

                      ] $ map
                      ( \card -> HH.img [ HP.src $ "./assets/" <> (cardImageUri card) ]
                      )
                      pile
                ) <$> maybePile
              )
        )
        fPiles
    )

tableau :: forall cs m. Array (Maybe Pile) -> H.ComponentHTML Action cs m
tableau tPiles =
  HH.div
    [ HP.class_ $ HH.ClassName "slot tableau"
    , HP.style "align-items: start;"
    ]
    ( map
        ( \maybePile -> fromMaybe emptySlot
            ( ( \pile ->
                  HH.div
                    [ HP.class_ $ HH.ClassName "tableau-pile"
                    , HE.onDragEnter DragEnter
                    , HE.onDragOver DragOver
                    , HE.onDrop DropCard
                    ]
                    ( map
                        ( \card ->
                            HH.div
                              [ HP.draggable true
                              , HE.onDragStart DragStart
                              ]
                              [ HH.img [ HP.src $ "./assets/" <> (cardImageUri card), HP.draggable false ] ]
                        )
                        pile
                    )
              ) <$> maybePile
            )
        )
        tPiles
    )

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div
    [ HP.class_ $ HH.ClassName "container" ]
    [
      -- Stock 
      renderStock state.stock

    -- waste 
    , waste state.waste

    -- Foundations
    , foundations state.foundations

    -- Tableau
    , tableau state.tableau
    ]

component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }


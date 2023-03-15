module App.Game where

import Prelude

import Data.Array (head, index, length, mapWithIndex, replicate, reverse, splitAt, tail, (..), (:))
import Data.Foldable (foldr)
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toLower, joinWith)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (tuple2)
import Effect (Effect, foreachE)
import Effect.Aff.Compat (EffectFn1, runEffectFn1)
import Effect.Class (class MonadEffect)
import Effect.Console (log, logShow)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.HTML.Event.DataTransfer (setDragImage)
import Web.HTML.Event.DragEvent (DragEvent, dataTransfer, toEvent)
import Web.HTML.HTMLImageElement (create, setSrc, toElement)
import Web.UIEvent.MouseEvent (MouseEvent)

foreign import _consoleLog :: forall a. EffectFn1 a Unit

consoleLog :: forall a. a -> Effect Unit
consoleLog = runEffectFn1 _consoleLog

--------------------------------------------------------------------------------------------------------
------------------------------------------------ MODEL -------------------------------------------------
--------------------------------------------------------------------------------------------------------

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

data Card
  = NormalCard NormalCard
  | Joker CardColour

instance showCard :: Show Card where
  show (NormalCard c) = show c
  show (Joker Red) = "Red joker"
  show (Joker Black) = "Black joker"

instance eqCard :: Eq Card where
  eq (Joker col1) (Joker col2) = col1 == col2
  eq (NormalCard nc1) (NormalCard nc2) = nc1 == nc2
  eq _ _ = false

type Pile = Array Card

type State =
  { stock :: Pile
  -- 7 Piles with only the top card face up. Boolean shows if the card is flipped
  , tableau :: Array (Array (Tuple Card Boolean))
  -- a pile where cards are dealt from the stock 
  , waste :: Pile
  -- 4 stacks of cards, one for each suit in ascending order
  , foundations :: Array Pile
  -- Card currently being dragged and the id of the origin pile
  , dragTarget :: Maybe { card :: Card, cardId :: CardId }
  }

initialState :: forall input. input -> State
initialState _ =
  { stock
  , tableau: map flippedTopCard tableau
  , waste: []
  , foundations: replicate 4 []
  , dragTarget: Nothing
  }
  where
  { tableau, stock } = splitDecktoTableauAndStock orderedDeck

flippedTopCard :: Pile -> Array (Tuple Card Boolean)
flippedTopCard [] = []
flippedTopCard cards =
  append
    (fromMaybe [] $ map (\topCard -> [ Tuple topCard true ]) (head $ reverse cards))
    (fromMaybe [] $ map (\rest -> map (\card -> Tuple card false) rest) (tail $ reverse cards))

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

splitDecktoTableauAndStock :: Pile -> { tableau :: Array Pile, stock :: Pile }
splitDecktoTableauAndStock deck =
  { tableau: res.before
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
-- TODO: Rename Not really cardid it is more a pileId
data CardId
  = FoundationId Int
  | TableauId Int
  | Waste

instance showCardId :: Show CardId where
  show (FoundationId id) = "foundation " <> (toStringAs decimal id)
  show (TableauId id) = "tableau " <> (toStringAs decimal id)
  show Waste = "waste"

instance eqCardId :: Eq CardId where
  eq (FoundationId i) (FoundationId j) = i == j
  eq (TableauId i) (TableauId j) = i == j
  eq (Waste) (Waste) = true
  eq _ _ = false

data Action
  = NoOp
  | LoadImages
  | DragStart CardId DragEvent
  | DragEnter CardId DragEvent
  | DragOver CardId DragEvent
  | DragLeave CardId DragEvent
  | DropCard CardId DragEvent
  | DealFromStock MouseEvent

loadImages :: forall o m. MonadEffect m => H.HalogenM State Action () o m Unit
loadImages = H.liftEffect $
  foreachE
    orderedDeck
    ( \card -> do
        img <- create
        setSrc ("./assets/" <> cardImageUri card) img
    )

handleAction :: forall output m. MonadEffect m => Action → H.HalogenM State Action () output m Unit
handleAction = case _ of
  LoadImages -> loadImages
  DragStart cardId de -> do
    state <- H.get
    let draggedCardUri = map (cardImageUri <<< _.card) $ getDragTarget cardId state
    H.liftEffect do
      log $ "drag start " <> (show cardId)
      img <- create
      -- TODO: the result of this is a maybe that indicates whether the operation
      --    was successfull. Use it for something
      _ <- sequence $ setSrc <$> (Just "./assets/" <> draggedCardUri) <*> Just img
      setDragImage (dataTransfer de) (toElement img) 10 10

    H.modify_
      ( \st ->
          case cardId of
            FoundationId i ->
              st
                { dragTarget = do
                    pile <- index st.foundations i
                    card <- head pile
                    pure { card, cardId }
                }
            TableauId i ->
              st
                { dragTarget = do
                    pile <- index st.tableau i
                    (Tuple card _) <- head pile
                    pure { card, cardId }
                }
            Waste -> st
              { dragTarget = do
                  card <- head st.waste
                  pure { card, cardId }
              }
      )

  DragEnter cardId _ -> do
    H.modify_
      ( \st ->
          case st.dragTarget of
            Just { cardId: TableauId i }
              |  cardId == (TableauId i)  &&
                  (map _.card st.dragTarget == bind (index st.tableau i) ((map fst) <<< head)) ->
                  st
                    { tableau =
                        mapWithIndex
                          (\j x -> if i == j then fromMaybe [] $ tail x else x)
                          st.tableau
                    }
            Just { cardId: FoundationId i } -> 
              st 
            Just { cardId: Waste } -> 
              st
            _ -> st
      )

  DragOver (TableauId i) e -> do
    { dragTarget } <- H.get
    case dragTarget of
      Just { cardId: (TableauId targetId) } | targetId == i -> pure unit
      _ -> H.liftEffect $ preventDefault $ toEvent e

  DragOver (FoundationId i) e -> do
    { dragTarget } <- H.get
    case dragTarget of
      Just { cardId: (FoundationId targetId) } | targetId == i -> pure unit
      _ -> H.liftEffect do
        preventDefault $ toEvent e

  -- not a valid drop target
  DragOver Waste _ -> pure unit

  DragLeave _ _ -> H.liftEffect do
    log "drag leave"

  DropCard pileId _ -> do
    H.modify_
      ( \s ->
          case Tuple pileId s.dragTarget of
            Tuple (TableauId i) (Just { card }) ->
              s
                { tableau = mapWithIndex
                    ( \j pile ->
                        if i == j then
                          (Tuple card true) : pile
                        else pile
                    )
                    s.tableau
                , dragTarget = Nothing
                }
            Tuple (FoundationId i) (Just { card }) -> s
              { foundations = mapWithIndex
                  ( \j pile ->
                      if i == j then
                        card : pile
                      else pile
                  )
                  s.foundations
              , dragTarget = Nothing
              }
            _ -> s
      )

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

getDragTarget :: CardId -> State -> Maybe { card :: Card, cardId :: CardId }
getDragTarget cardId { foundations, tableau, waste } =
  case cardId of
    FoundationId i -> do
      pile <- index foundations i
      card <- head pile
      pure { card, cardId }
    TableauId i -> do
      pile <- index tableau i
      (Tuple card _) <- head pile
      pure { card, cardId }
    Waste -> do
      card <- head waste
      pure { card, cardId }

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

renderWaste :: forall cs m. Pile -> H.ComponentHTML Action cs m
renderWaste wastePile =
  HH.div
    [ HP.class_ $ HH.ClassName "slot waste"
    , HE.onDragStart $ DragStart Waste
    ]
    [ fromMaybe emptySlot ((\topCard -> HH.img [ HP.src $ "./assets/" <> (cardImageUri $ topCard) ]) <$> (head wastePile)) ]

renderFoundations :: forall cs m. Array Pile -> H.ComponentHTML Action cs m
renderFoundations fPiles =
  HH.div
    [ HP.class_ $ HH.ClassName "foundations slot" ]
    ( mapWithIndex
        ( \i pile ->
            HH.div
              [ HE.onDragEnter $ DragEnter $ FoundationId i
              , HE.onDragOver $ DragOver $ FoundationId i
              , HE.onDragLeave $ DragLeave $ FoundationId i
              , HE.onDrop $ DropCard $ FoundationId i
              ]
              [ fromMaybe emptySlot
                  $ map
                      ( \card ->
                          HH.img [ HP.draggable false, HP.src $ "./assets/" <> (cardImageUri card) ]
                      )
                  $ head pile
              ]
        )
        fPiles
    )

renderTableau :: forall cs m. Array (Array (Tuple Card Boolean)) -> Maybe Int -> H.ComponentHTML Action cs m
renderTableau tPiles dragId =
  HH.div
    [ HP.class_ $ HH.ClassName "slot tableau"
    , HP.style "align-items: start;"
    ]
    ( mapWithIndex
        ( \i ->
            if (dragId == Just i) then
              renderPile true i
            else
              renderPile false i
        )
        tPiles
    )

renderPile :: forall cs m. Boolean -> Int -> Array (Tuple Card Boolean) -> H.ComponentHTML Action cs m
renderPile _ i [] = HH.div
  [ HE.onDragEnter $ DragEnter $ TableauId i
  , HE.onDragOver $ DragOver $ TableauId i
  , HE.onDragLeave $ DragLeave $ FoundationId i
  , HE.onDrop $ DropCard $ TableauId i
  ]
  [ emptySlot ]
renderPile topCardHidden i pile =
  HH.div
    [ HP.class_ $ HH.ClassName "tableau-pile"
    , HE.onDragEnter $ DragEnter $ TableauId i
    , HE.onDragLeave $ DragLeave $ FoundationId i
    , HE.onDragOver $ DragOver $ TableauId i
    , HE.onDrop $ DropCard $ TableauId i
    ]
    ( mapWithIndex
        ( \j (Tuple card flipped) ->
            case flipped of
              true ->
                HH.div
                  [ HP.draggable true
                  , HE.onDragStart $ DragStart $ TableauId i
                  -- Hide the top card if it is being dragged
                  , HP.style
                      if j == (length pile - 1) && topCardHidden then
                        "transition:0.01s; transform:translateX(-9999px)"
                      else
                        ""
                  ]
                  [ HH.img [ HP.src $ "./assets/" <> (cardImageUri card), HP.draggable false ] ]
              false -> HH.div
                [ HP.draggable false
                -- , HP.selectable false
                , HE.onDragStart $ DragStart $ TableauId i
                ]
                [ HH.img [ HP.draggable false, HP.src $ "./assets/" <> (backCardFace) ] ]

        )
        (reverse pile)
    )

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div
    [ HP.class_ $ HH.ClassName "container" ]
    [
      -- Stock 
      renderStock state.stock

    -- waste 
    , renderWaste state.waste

    -- Foundations
    , renderFoundations state.foundations

    -- Tableau
    , renderTableau state.tableau (map (f <<< _.cardId) state.dragTarget)
    ]
  where
  f (TableauId i) = i
  f _ = -1

component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just LoadImages
        }
    }


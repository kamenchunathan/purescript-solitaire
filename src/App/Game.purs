module App.Game where

import Prelude

import Data.Array (head, index, mapWithIndex, replicate, reverse, splitAt, tail, (..), (:))
import Data.Foldable (foldr)
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toLower, joinWith)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (foreachE)
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
data CardId
  = FoundationId Int
  | TableauId Int
  | Waste

instance showId :: Show CardId where
  show (FoundationId id) = "foundation " <> (toStringAs decimal id)
  show (TableauId id) = "tableau " <> (toStringAs decimal id)
  show Waste = "waste"

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
    -- FIXME(nathan): When cache is disabled drag image is not set properly.
    --    Something to do with fetching the image first. This is ideally should
    --    not be a problem when dragging for the first time because image is
    --    already loaded as it has to be flipped before it can be dragged.
    --    However explore different caching solutions such as loading all images
    --    in advance or storing a reference to the image
    -- draggedCard <- H.gets $ (map $ cardImageUri <<< _.card) <<< _.dragTarget
    state <- H.get
    let draggedCardUri = map (cardImageUri <<< _.card) $ getDragTarget cardId state
    H.liftEffect do
      log $ "drag start " <> (show cardId)
      img <- create
      -- TODO: the result of this is a maybe that indicates whether the operation
      --    was successfull. Use it for something
      _ <- sequence $ setSrc <$> (Just "./assets/" <> draggedCardUri) <*> Just img
      setDragImage (dataTransfer de) (toElement img) 10 10

    -- H.modify_
    --   ( \st ->
    --       case cardId of
    --         FoundationId i ->
    --           st
    --             { dragTarget = do
    --                 pile <- index st.foundations i
    --                 card <- head pile
    --                 pure { card, cardId }
    --             , foundations = fromMaybe [] do
    --                 pile <- index st.foundations i
    --                 _ <- tail pile
    --                 pure []
    --             }
    --         TableauId i ->
    --           st
    --             { dragTarget = do
    --                 pile <- index st.tableau i
    --                 (Tuple card _) <- head pile
    --                 pure { card, cardId }
    --             , tableau =
    --                 mapWithIndex
    --                   (\j x -> if i == j then fromMaybe [] $ tail x else x)
    --                   st.tableau
    --             }
    --         Waste -> st
    --           { dragTarget = do
    --               card <- head st.waste
    --               pure { card, cardId }
    --           }
    --   )

  DragEnter _ e -> H.liftEffect do
    preventDefault $ toEvent e
    log "drag enter"

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

  DropCard i _ -> do
    H.liftEffect $ logShow i

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

renderTableau :: forall cs m. Array (Array (Tuple Card Boolean)) -> H.ComponentHTML Action cs m
renderTableau tPiles =
  HH.div
    [ HP.class_ $ HH.ClassName "slot tableau"
    , HP.style "align-items: start;"
    ]
    (mapWithIndex renderPile tPiles)

renderPile :: forall cs m. Int -> Array (Tuple Card Boolean) -> H.ComponentHTML Action cs m
renderPile i [] = HH.div
  [ HE.onDragEnter $ DragEnter $ TableauId i
  , HE.onDragOver $ DragOver $ TableauId i
  , HE.onDragLeave $ DragLeave $ FoundationId i
  , HE.onDrop $ DropCard $ TableauId i
  ]
  [ emptySlot ]
renderPile i pile =
  HH.div
    [ HP.class_ $ HH.ClassName "tableau-pile"
    , HE.onDragEnter $ DragEnter $ TableauId i
    , HE.onDragLeave $ DragLeave $ FoundationId i
    , HE.onDragOver $ DragOver $ TableauId i
    , HE.onDrop $ DropCard $ TableauId i
    ]
    ( map
        ( \(Tuple card flipped) ->
            case flipped of
              true ->
                HH.div
                  [ HP.draggable true
                  , HE.onDragStart $ DragStart $ TableauId i
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
    , renderTableau state.tableau
    ]

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


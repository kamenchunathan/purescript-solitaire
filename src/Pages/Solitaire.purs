module Pages.Solitaire (component, Action) where

import Prelude

import Data.Array (head, index, length, mapWithIndex, replicate, reverse, splitAt, tail, (..), (:))
import Data.Card (Card(..), CardColour(..), Suit(..), Value(..))
import Data.Foldable (foldr)
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toLower, joinWith)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (foreachE)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
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
  | DragEnd CardId DragEvent
  | DealFromStock MouseEvent

loadImages :: forall o m. MonadEffect m => H.HalogenM State Action () o m Unit
loadImages = H.liftEffect $
  foreachE
    orderedDeck
    ( \card -> do
        img <- create
        setSrc ("./assets/" <> cardImageUri card) img
    )

handleAction
  :: forall output m
   . MonadEffect m
  => Action
  → H.HalogenM State Action () output m Unit
handleAction = case _ of
  LoadImages -> loadImages

  DragStart cardId de -> do
    state <- H.get
    let draggedCardUri = map (cardImageUri <<< _.card) $ getDragTarget cardId state
    -- Set the drag image 
    H.liftEffect do
      img <- create
      -- TODO: the result of this is a maybe that indicates whether the operation
      --    was successfull. Use it for something
      _ <- sequence $ setSrc <$> (Just "./assets/" <> draggedCardUri) <*> Just img
      setDragImage (dataTransfer de) (toElement img) 10 10

    -- Set the drag target into the state
    { dragTarget } <- H.modify
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
    H.liftEffect $ log $ "Drag start: " <> (show dragTarget)

  DragEnter id _ -> do
    -- NOTE: might be a good place to add visual indications if it is a valid drop
    -- target
    log $ "Drag Enter: " <> (show id)

  -- NOTE: preventing default on drag over identifies the event target as a
  -- valid drop target.
  -- This would be a wonderful place to add game logic that uses state to determine
  -- if the card can be added to a specific pile
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

  DragLeave id _ -> H.liftEffect do
    log $ "Drag Leave: " <> (show id)

  DropCard pileId _ -> do
    log $ "Drop card: " <> (show pileId)

    -- Add the card that was being dragged to the current drop target pile
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
                }
            Tuple (FoundationId i) (Just { card }) -> s
              { foundations = mapWithIndex
                  ( \j pile ->
                      if i == j then
                        card : pile
                      else pile
                  )
                  s.foundations
              }
            _ -> s
      )

    -- Remove the card being dragged from it's original pile. 
    -- NOTE: This is where the card is removed. It was originally hidden
    -- using style when the drag started but is still in it's pile in the
    -- DOM
    H.modify_
      ( \(s@{ dragTarget }) ->
          let
            st = s { dragTarget = Nothing }
          in
            case dragTarget of
              Just { cardId: (FoundationId originPileId) } ->
                st
                  { foundations = mapWithIndex
                      ( \i p ->
                          if i == originPileId then fromMaybe [] $ tail p
                          else p
                      )
                      st.foundations
                  }
              Just { cardId: (TableauId originPileId) } ->
                st
                  { tableau = mapWithIndex
                      ( \i p ->
                          if i == originPileId then mapWithIndex
                            ( \j (Tuple c flipped) ->
                                if j == 0 then (Tuple c true)
                                else (Tuple c flipped)
                            )
                            (fromMaybe [] $ tail p)
                          else p
                      )

                      st.tableau
                  }
              Just { cardId: Waste } -> st { waste = fromMaybe [] $ tail st.waste }
              _ -> st
      )

  DragEnd cardId _ -> do
    log $ "Drag ended: " <> (show cardId)
    H.modify_ (\st -> st { dragTarget = Nothing })

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

renderFoundations
  :: forall cs m
   . Array Pile
  -> H.ComponentHTML Action cs m
renderFoundations fPiles =
  HH.div
    [ HP.class_ $ HH.ClassName "foundations slot" ]
    ( mapWithIndex
        ( \i pile ->
            HH.div
              [ HP.draggable true
              , HE.onDragStart $ DragStart $ FoundationId i
              , HE.onDragEnter $ DragEnter $ FoundationId i
              , HE.onDragOver $ DragOver $ FoundationId i
              , HE.onDragLeave $ DragLeave $ FoundationId i
              , HE.onDrop $ DropCard $ FoundationId i
              , HE.onDragEnd $ DragEnd $ FoundationId i
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

renderTableau
  :: forall cs m
   . Array (Array (Tuple Card Boolean))
  -> Maybe Int
  -> H.ComponentHTML Action cs m
renderTableau tPiles dragId =
  HH.div
    [ HP.class_ $ HH.ClassName "slot tableau"
    , HP.style "align-items: start;"
    ]
    ( mapWithIndex
        (\i -> renderPile (dragId == Just i) i)
        tPiles
    )

renderCardImage
  :: forall cs m r
   . { card :: Card
     , flipped :: Boolean
     , hide :: Boolean
     | r
     }
  -> H.ComponentHTML Action cs m
renderCardImage { card, hide, flipped } =
  HH.img
    [ HP.src
        if flipped then "./assets/" <> (cardImageUri card)
        else "./assets/" <> backCardFace
    , HP.draggable false
    , HP.style
        if hide then "transition:0.01s; transform:translateX(-9999px)"
        else ""
    ]

-- TODO: if the pile is empty and the top card is to be hidden display the widget
-- for an empty pile
renderPile
  :: forall cs m
   . Boolean
  -> Int
  -> Array (Tuple Card Boolean)
  -> H.ComponentHTML Action cs m
renderPile _ pileId [] = HH.div
  [ HE.onDragEnter $ DragEnter $ TableauId pileId
  , HE.onDragOver $ DragOver $ TableauId pileId
  , HE.onDragLeave $ DragLeave $ TableauId pileId
  , HE.onDrop $ DropCard $ TableauId pileId
  , HE.onDragEnd $ DragEnd $ TableauId pileId
  ]
  [ emptySlot ]
renderPile hideTopCard pileId pile =
  HH.div
    [ HP.class_ $ HH.ClassName "tableau-pile"
    , HE.onDragEnter $ DragEnter $ TableauId pileId
    , HE.onDragLeave $ DragLeave $ TableauId pileId
    , HE.onDragOver $ DragOver $ TableauId pileId
    , HE.onDrop $ DropCard $ TableauId pileId
    , HE.onDragEnd $ DragEnd $ TableauId pileId
    ]
    ( mapWithIndex
        ( \i (Tuple card flipped) ->
            case flipped of
              true ->
                HH.div
                  [ HP.draggable true
                  , HE.onDragStart $ DragStart $ TableauId pileId
                  ]
                  [ renderCardImage
                      { card
                      , flipped
                      , hide: hideTopCard && i == (length pile - 1)
                      }
                  ]
              false -> HH.div
                [ HP.draggable false
                , HE.onDragStart $ DragStart $ TableauId pileId
                ]
                [ renderCardImage { card, flipped, hide: false } ]
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


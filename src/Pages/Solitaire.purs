module Pages.Solitaire (component, Action) where

import Prelude

import Component.Navbar (navbar)
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

type Pile a = Array (Card a)

type TableauInfo = { flipped :: Boolean }

type State =
  {
    -- A stack of cards all face down that can be moved to the waste a number
    -- of times
    stock :: Pile Unit

  -- 7 Piles with only the top card face up. Boolean shows if the card is flipped
  , tableau :: Array (Pile TableauInfo)

  -- a pile where cards are dealt from the stock 
  , waste :: Pile Unit

  -- 4 stacks of cards, one for each suit in ascending order
  , foundations :: Array (Pile Unit)

  -- Card currently being dragged and the id of the origin pile
  , dragTarget ::
      Maybe
        { card :: Card Unit
        , pileId :: PileId
        }
  }

initialState :: forall input. input -> State
initialState _ =
  { stock
  , tableau: map withFlippedTopCard (map (\a -> map withDefaultTableauInfo a) tableau)
  , waste: []
  , foundations: replicate 4 []
  , dragTarget: Nothing
  }
  where
  { tableau, stock } = splitDecktoTableauAndStock orderedDeck

orderedDeck :: Pile Unit
orderedDeck =
  [ Joker Black unit, Joker Red unit ]
    <> singleSuit Spades
    <> singleSuit Diamonds
    <> (reverse $ singleSuit Clubs)
    <> (reverse $ singleSuit Hearts)

singleSuit :: Suit -> Pile Unit
singleSuit suit =
  [ NormalCard { value: Ace, suit: suit } unit
  , NormalCard { value: Num 2, suit: suit } unit
  , NormalCard { value: Num 3, suit: suit } unit
  , NormalCard { value: Num 4, suit: suit } unit
  , NormalCard { value: Num 5, suit: suit } unit
  , NormalCard { value: Num 6, suit: suit } unit
  , NormalCard { value: Num 7, suit: suit } unit
  , NormalCard { value: Num 8, suit: suit } unit
  , NormalCard { value: Num 9, suit: suit } unit
  , NormalCard { value: Num 10, suit: suit } unit
  , NormalCard { value: Jack, suit: suit } unit
  , NormalCard { value: Queen, suit: suit } unit
  , NormalCard { value: King, suit: suit } unit
  ]

splitDecktoTableauAndStock
  :: forall a
   . (Pile a)
  -> { tableau :: Array (Pile a), stock :: (Pile a) }
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

discardInfo :: forall a. Card a -> Card Unit
discardInfo card =
  case card of
    NormalCard a _ -> NormalCard a unit
    Joker a _ -> Joker a unit

withDefaultTableauInfo :: forall a. Card a -> Card TableauInfo
withDefaultTableauInfo = case _ of
  NormalCard a _ -> NormalCard a { flipped: false }
  Joker a _ -> Joker a { flipped: false }

withFlippedTopCard
  :: Pile TableauInfo
  -> Pile TableauInfo
withFlippedTopCard [] = []
withFlippedTopCard cards =
  let
    faceup (NormalCard a _) = [ NormalCard a { flipped: true } ]
    faceup (Joker col _) = [ Joker col { flipped: true } ]

    facedown = map
      ( \card ->
          case card of
            NormalCard a _ -> NormalCard a { flipped: false }
            Joker col _ -> Joker col { flipped: false }
      )

  in
    append
      (fromMaybe [] $ map faceup (head $ reverse cards))
      (fromMaybe [] $ map facedown (tail $ reverse cards))

getFlipped :: forall r. Card { flipped :: Boolean | r } -> Boolean
getFlipped = case _ of
  Joker _ { flipped } -> flipped
  NormalCard _ { flipped } -> flipped

------------------------------------------------ UPDATE -------------------------------------------------
-- TODO: Rename Not really cardid it is more a pileId
data PileId
  = FoundationId Int
  | TableauId Int
  | Waste

instance showPileId :: Show PileId where
  show (FoundationId id) = "foundation " <> (toStringAs decimal id)
  show (TableauId id) = "tableau " <> (toStringAs decimal id)
  show Waste = "waste"

instance eqPileId :: Eq PileId where
  eq (FoundationId i) (FoundationId j) = i == j
  eq (TableauId i) (TableauId j) = i == j
  eq (Waste) (Waste) = true
  eq _ _ = false

data Action
  = NoOp
  | LoadImages
  | DragStart PileId DragEvent
  | DragEnter PileId DragEvent
  | DragOver PileId DragEvent
  | DragLeave PileId DragEvent
  | DropCard PileId DragEvent
  | DragEnd PileId DragEvent
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

  DragStart pileId de -> do
    state <- H.get
    let draggedCardUri = map (cardImageUri <<< _.card) $ getDragTarget pileId state
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
          case pileId of
            FoundationId i ->
              st
                { dragTarget = do
                    pile <- index st.foundations i
                    card <- head pile
                    pure { card, pileId }
                }
            TableauId i ->
              st
                { dragTarget = do
                    pile <- index st.tableau i
                    card <- discardInfo <$> head pile
                    pure { card, pileId }
                }
            Waste -> st
              { dragTarget = do
                  card <- head st.waste
                  pure { card, pileId }
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
      Just { pileId: (TableauId targetId) } | targetId == i -> pure unit
      _ -> H.liftEffect $ preventDefault $ toEvent e

  DragOver (FoundationId i) e -> do
    { dragTarget } <- H.get
    case dragTarget of
      Just { pileId: (FoundationId targetId) } | targetId == i -> pure unit
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
                          ((\r -> r { flipped = true }) <$> withDefaultTableauInfo card) : pile
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
              Just { pileId: (FoundationId originPileId) } ->
                st
                  { foundations = mapWithIndex
                      ( \i p ->
                          if i == originPileId then fromMaybe [] $ tail p
                          else p
                      )
                      st.foundations
                  }
              Just { pileId: (TableauId originPileId) } ->
                st
                  { tableau = mapWithIndex
                      ( \i p ->
                          if i == originPileId then mapWithIndex
                            ( \j card ->
                                if j == 0 then
                                  (\r -> r { flipped = true }) <$> card
                                else 
                                  card
                            )
                            (fromMaybe [] $ tail p)
                          else p
                      )

                      st.tableau
                  }
              Just { pileId: Waste } -> st { waste = fromMaybe [] $ tail st.waste }
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

getDragTarget
  :: PileId
  -> State
  -> Maybe
       { card :: (Card Unit)
       , pileId :: PileId
       }
getDragTarget pileId { foundations, tableau, waste } =
  case pileId of
    FoundationId i -> do
      pile <- index foundations i
      card <- head pile
      pure { card, pileId }
    TableauId i -> do
      pile <- index tableau i
      card <- head $ discardInfo <$> pile
      pure { card, pileId }
    Waste -> do
      card <- head waste
      pure { card, pileId }

------------------------------------------------ RENDER -------------------------------------------------

cardImageUri :: forall a. Card a -> String
cardImageUri (NormalCard c _) =
  (toLower $ joinWith "_" [ show c.value, show c.suit, "white" ]) <> ".png"
cardImageUri (Joker _ _) =
  "joker_white.png"

-- ( toLower $ joinWith "_" ["joker", show col ]) <> ".png"

backCardFace :: String
backCardFace = "back_blue_basic.png"

emptySlot :: forall cs m. H.ComponentHTML Action cs m
emptySlot =
  HH.div [ HP.class_ $ HH.ClassName "bg-tuscany h-[84px] w-[60px]" ] []

renderStock
  :: forall cs m
   . Pile Unit
  -> H.ComponentHTML Action cs m
renderStock [] =
  HH.div
    [ HP.class_ $ HH.ClassName ""
    , HE.onClick DealFromStock
    ]
    [ emptySlot ]

renderStock _ =
  HH.div
    [ HP.class_ $ HH.ClassName ""
    , HP.draggable false
    , HE.onClick DealFromStock
    ]
    [ HH.img
        [ HP.draggable false
        , HP.src $ "./assets/" <> (backCardFace)
        ]
    ]

renderWaste :: forall cs m. Pile Unit -> H.ComponentHTML Action cs m
renderWaste wastePile =
  HH.div
    [ HP.class_ $ HH.ClassName "col-start-2 "
    , HE.onDragStart $ DragStart Waste
    ]
    [ fromMaybe emptySlot ((\topCard -> HH.img [ HP.src $ "./assets/" <> (cardImageUri $ topCard) ]) <$> (head wastePile)) ]

renderFoundations
  :: forall cs m
   . Array (Pile Unit)
  -> H.ComponentHTML Action cs m
renderFoundations fPiles =
  HH.div
    [ HP.class_ $ HH.ClassName "col-start-3 flex gap-4 " ]
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
   . Array (Pile TableauInfo)
  -> Maybe Int
  -> H.ComponentHTML Action cs m
renderTableau tPiles dragId =
  HH.div
    [ HP.class_ $ HH.ClassName $
        "row-start-2 col-start-1 row-end-5 col-end-4 flex justify-between"
    ]
    ( mapWithIndex
        (\i -> renderTableauPile (dragId == Just i) i)
        tPiles
    )

renderCardImage
  :: forall a cs m r
   . { card :: Card a
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
renderTableauPile
  :: forall cs m
   . Boolean
  -> Int
  -> Pile TableauInfo
  -> H.ComponentHTML Action cs m
renderTableauPile _ pileId [] = HH.div
  [ HE.onDragEnter $ DragEnter $ TableauId pileId
  , HE.onDragOver $ DragOver $ TableauId pileId
  , HE.onDragLeave $ DragLeave $ TableauId pileId
  , HE.onDrop $ DropCard $ TableauId pileId
  , HE.onDragEnd $ DragEnd $ TableauId pileId
  ]
  [ emptySlot ]
renderTableauPile hideTopCard pileId pile =
  HH.div
    [ HP.class_ $ HH.ClassName "tableau-pile"
    , HE.onDragEnter $ DragEnter $ TableauId pileId
    , HE.onDragLeave $ DragLeave $ TableauId pileId
    , HE.onDragOver $ DragOver $ TableauId pileId
    , HE.onDrop $ DropCard $ TableauId pileId
    , HE.onDragEnd $ DragEnd $ TableauId pileId
    ]
    ( mapWithIndex
        ( \i card ->
            if getFlipped card then
              HH.div
                [ HP.draggable true
                , HE.onDragStart $ DragStart $ TableauId pileId
                ]
                [ renderCardImage
                    { card
                    , flipped: getFlipped card
                    , hide: hideTopCard && i == (length pile - 1)
                    }
                ]
            else
              HH.div
                [ HP.draggable false
                , HE.onDragStart $ DragStart $ TableauId pileId
                ]
                [ renderCardImage
                    { card
                    , flipped: getFlipped card
                    , hide: false
                    }
                ]
        )
        (reverse pile)
    )

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div
    [ HP.class_ $ HH.ClassName $
        "bg-xanadu min-h-screen overflow-hidden pb-8"
    ]
    [ navbar [ "Solitaire" ]
    , HH.div
        [ HP.class_ $ HH.ClassName $
            "h-full grid grid-cols-3 grid-rows-4 w-5/6 lg:w-3/6 mx-auto"
        ]
        [ -- Stock 
          renderStock state.stock

        -- waste 
        , renderWaste state.waste

        -- Foundations
        , renderFoundations state.foundations

        -- Tableau
        , renderTableau state.tableau (map (f <<< _.pileId) state.dragTarget)
        ]
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


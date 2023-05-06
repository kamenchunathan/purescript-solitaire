module Pages.Home (component, Action) where

import Prelude

import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data Action = NoOp

type State = Unit

initialState :: forall a. a -> State
initialState _ = unit

render :: forall cs m. State -> H.ComponentHTML Action cs m
render _ =
  HH.div
    [ HP.class_ $ ClassName "bg-xanadu min-h-screen" ]
    [ -- header
      HH.div
        [ HP.class_ $ ClassName $ "w-5/6 py-4 mx-auto text-emerald-50 "
            <> "font-semibold text-3xl flex justify-between items-end"
        ]
        [ HH.a
            [ HP.href "/" ]
            [ HH.text "A-Team ♠" ]
        , HH.a
            [ HP.class_ $ ClassName "text-2xl"
            , HP.href "/"
            ]
            [ HH.text "Get Started" ]
        ]
    -- Main banner
    , HH.div
        [ HP.class_ $ ClassName "flex items-center justify-evenly mt-24" ]
        [ HH.div
            [ HP.class_ $ ClassName "" ]
            [ HH.img
                [ HP.src "/assets/banner-cards.png"
                , HP.class_ $ ClassName "h-64"
                ]
            ]
        , HH.div
            [ HP.class_ $ ClassName "py-8 text-emerald-50" ]
            [ HH.p
                [ HP.class_ $ ClassName "text-4xl font-semibold" ]
                [ HH.text "Ati umesema uko " ]
            , HH.p
                [ HP.class_ $ ClassName "text-8xl font-semibold underline" ]
                [ HH.text "Kadi?" ]
            , HH.div
                [ HP.class_ $ ClassName "flex flex-col pt-8" ]
                [ HH.p
                    [ HP.class_ $ ClassName "py-2 text-2xl" ]
                    [ HH.text "A website for playing card games." ]
                , HH.a
                    [ HP.class_ $ ClassName $ "inline-block p-2 px-4 m-2 ml-auto rounded-md" <>
                        " text-2xl font-semibold bg-charleston-green shadow"
                    , HP.href "/#games"
                    ]
                    [ HH.text "Try it Out!" ]
                ]
            ]
        ]
    , HH.div
        [ HP.id "games"
        , HP.class_ $ ClassName "bg-xiketic mt-24 pb-8"
        ]
        [ HH.div
            [ HP.class_ $ ClassName "w-5/6 mx-auto text-emerald-50 pt-4" ]
            [ HH.p
                [ HP.class_ $ ClassName "py-4 text-3xl" ]
                [ HH.text "Take a look at some of the games we have"
                ]
            , HH.a
                [ HP.class_ $ ClassName "block text-3xl px-4 py-8 underline font-semibold"
                , HP.href "/solitaire"
                ]
                [ HH.text "Solitaire" ]
            , HH.p
                [ HP.class_ $ ClassName "text-xl px-4" ]
                [ HH.text "A single player card game where you try to arrange cards" ]
            , HH.a
                [ HP.class_ $ ClassName $ "block w-fit text-2xl py-2 px-4 ml-auto mr-8"
                    <> " rounded-md bg-xanadu text-xiketic font-semibold"
                , HP.href "/solitaire"
                ]
                [ HH.text "Play" ]

            ]
        ]
    ]

handleAction :: forall t11 f12. Applicative f12 => t11 -> f12 Unit
handleAction _ = pure unit

component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      }
  }

module Component.Navbar (navbar) where

import Prelude

import Data.Array (foldMap)
import Halogen (ClassName(..))
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

navbar :: forall a b. Array String -> HTML a b
navbar path = HH.div
  [ HP.class_ $ ClassName $ "w-5/6 py-4 mx-auto text-emerald-50 "
      <> "font-semibold text-2xl flex justify-between items-end"
  ]
  [ HH.div []
      [ HH.a
          [ HP.href "/" ]
          [ HH.text $ "A-Team ♠" ]
      , HH.div
          [ HP.class_ $ ClassName "text-lg inline-block" ]
          ( foldMap
              ( \s ->
                  [ HH.span
                      [ HP.class_ $ ClassName "inline-block px-4" ]
                      [ HH.text " ➤ " ]
                  , HH.span
                      [ HP.class_ $ ClassName "inline-block" ]
                      [ HH.text s ]

                  ]
              )
              path
          )
      ]

  , HH.a
      [ HP.class_ $ ClassName "text-xl"
      , HP.href "/"
      ]
      [ HH.text "Get Started" ]
  ]

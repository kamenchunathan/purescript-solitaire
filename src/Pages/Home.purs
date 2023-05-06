module Pages.Home (component, Action) where

import Prelude

import Halogen as H
import Halogen.HTML as HH

data Action = NoOp

data State = State

initialState :: forall t9. t9 -> State
initialState _ = State

render :: forall cs m. State -> H.ComponentHTML Action cs m 
render _ = HH.div [] [ HH.text "wow" ]

handleAction :: forall t11 f12. Applicative f12 => t11 -> f12 Unit
handleAction _ = pure unit

component :: forall q i o m . H.Component q i o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      }
  }

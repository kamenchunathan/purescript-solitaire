module Component.Router (component, Query(..)) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Route (Route(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Navigate (class Navigate, navigate)
import Pages.Solitaire as SolitairePage
import Pages.Home as HomePage
import Routing.PushState (PushStateInterface)
import Type.Prelude (Proxy(..))

type State =
  { current_route :: Maybe Route
  , nav :: PushStateInterface
  }

data Query a
  = Navigate Route a
  | LocalLinkClicked Route a

data Action
  = Initialize
  | HandleSolitairePageAction SolitairePage.Action
  | HandleHomePageAction HomePage.Action

type ChildSlots =
  ( solitairePage :: forall query. H.Slot query SolitairePage.Action Unit
  , homePage :: forall query. H.Slot query HomePage.Action Unit
  )

initialState :: PushStateInterface -> State
initialState nav =
  { current_route: Nothing
  , nav
  }

handleAction
  :: forall o m
   . MonadEffect m
  => MonadAff m
  => Action
  -> H.HalogenM State Action ChildSlots o m Unit
handleAction _ = pure unit

handleQuery
  :: forall a o m
   . MonadEffect m
  => Navigate m
  => Query a
  -> H.HalogenM State Action ChildSlots o m (Maybe a)
handleQuery = case _ of
  Navigate route a -> do
    mRoute <- H.gets _.current_route
    when (mRoute /= Just route) $ H.modify_ _ { current_route = Just route }
    pure (Just a)
  LocalLinkClicked route a -> do
    nav <- H.gets _.nav
    navigate nav route
    pure (Just a)

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
render st = case fromMaybe NotFound st.current_route of
  Home -> HH.slot (Proxy :: _ "homePage") unit HomePage.component unit HandleHomePageAction
  Solitaire -> HH.slot (Proxy :: _ "solitairePage") unit SolitairePage.component unit HandleSolitairePageAction
  NotFound -> HH.div [] [ HH.text "Page Not Found" ]

component
  :: forall o m
   . MonadEffect m
  => Navigate m
  => MonadAff m
  => H.Component Query PushStateInterface o m
component = H.mkComponent
  { initialState
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      }
  , render
  }


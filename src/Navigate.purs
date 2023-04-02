module Navigate where

import Prelude

import Data.Route (Route)
import Halogen (HalogenM, lift)
import Routing.PushState (PushStateInterface)

class Monad m <= Navigate m where
  navigate :: PushStateInterface -> Route -> m Unit

instance navigateHalogenM :: Navigate m => Navigate (HalogenM state action slots output m) where
  navigate nav route = lift (navigate nav route)

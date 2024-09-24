module AppM where

import Prelude

import Effect.Aff (Aff)
import Data.Route (routeCodec)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (unsafeToForeign)
import Navigate (class Navigate)
import Routing.Duplex (print)

newtype AppM a = AppM (Aff a)

runAppM :: AppM ~> Aff
runAppM (AppM m) = m

derive newtype instance  Functor AppM
derive newtype instance  Apply AppM
derive newtype instance  Applicative AppM
derive newtype instance  Bind AppM
derive newtype instance  Monad AppM
derive newtype instance  MonadEffect AppM
derive newtype instance  MonadAff AppM

instance Navigate AppM where
  navigate nav route = liftEffect $ nav.pushState (unsafeToForeign {}) (print routeCodec route)

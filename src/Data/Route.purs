module Data.Route where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Home
  | Solitaire
  | NotFound

derive instance Generic Route _
derive instance Eq Route
derive instance Ord Route

instance Show Route where
  show Home = "home"
  show Solitaire = "solitaire"
  show NotFound = "notfound"

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "Solitaire": "solitaire" / noArgs
  , "NotFound": "not-found" / noArgs
  }

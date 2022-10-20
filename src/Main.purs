module Main where

import Prelude

import App.Game as Game
import Effect.Class.Console (log)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  log "maajabu haya"
  body <- HA.awaitBody
  runUI Game.component unit body

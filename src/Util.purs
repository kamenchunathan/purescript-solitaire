module Util (consoleLog, uneffectfulLog) where

import Prelude

import Effect (Effect)
import Effect.Aff.Compat (EffectFn1, runEffectFn1)

foreign import _consoleLog :: forall a. EffectFn1 a Unit

foreign import _uneffectfulLog :: forall a. a -> Unit

consoleLog :: forall a. a -> Effect Unit
consoleLog = runEffectFn1 _consoleLog

uneffectfulLog :: forall a. a -> Unit
uneffectfulLog = _uneffectfulLog

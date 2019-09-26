module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)

foreign import mouse_event :: Effect String

main :: Effect Unit
main =
    mouse_event >>= \x ->
    logShow x

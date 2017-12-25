module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Math (sqrt, pi)
import Global (nan, isNaN)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = pi * r * r

isNanNan = isNaN nan

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow (circleArea 1.0)


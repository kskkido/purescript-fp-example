module Lib.Math where

import Prelude
import Math as Math

margin :: Number -> Number -> Number
margin x y = if y == 0.0 then x else -(1.0 - x / y)

lerp :: Number -> Number -> Number -> Number
lerp x y z = (1.0 - z) * x + z * y

lerpW :: Number -> Number -> Number -> Number -> Number
lerpW x y t z = lerp x y (if Math.abs (margin x y) < z then 1.0 else t)


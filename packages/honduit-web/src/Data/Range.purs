module Data.Range where

import Prelude
import Data.Tuple as Tuple

type Range = Tuple.Tuple Number Number

from :: Number -> Number -> Range
from x y = if x < y then Tuple.Tuple x y else Tuple.Tuple y x

distance :: Range -> Number
distance ra = Tuple.snd ra - Tuple.fst ra

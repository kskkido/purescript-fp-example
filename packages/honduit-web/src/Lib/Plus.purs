module Lib.Plus where

import Prelude
import Control.Alt as Alt
import Control.Plus as Plus
import Data.Foldable as Foldable

sum :: forall a b c. Alt.Alt a => Plus.Plus a => Foldable.Foldable c => c (a b) -> a b
sum = Foldable.foldl Alt.alt Plus.empty

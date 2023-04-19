module Data.AnimationStateAction where

import Data.AnimationState as AnimationState

data AnimationStateAction =
    Set AnimationState.AnimationState
  | Tween
      { target :: Number
      , speed :: Number
      , threshold :: Number
      }
  | Linear
      { target :: Number
      , duration :: Number
      }

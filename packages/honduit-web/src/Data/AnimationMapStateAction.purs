module Data.AnimationMapStateAction where

import Prelude
import Data.AnimationStateAction as AnimationStateAction

type AnimationMapStateAction =
  { key :: String
  , action :: AnimationStateAction.AnimationStateAction
  }

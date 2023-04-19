module Data.AnimationMapState where

import Prelude
import Data.Map as Map
import Data.AnimationState as AnimationState

type AnimationMapState = Map.Map
  String
  AnimationState.AnimationState


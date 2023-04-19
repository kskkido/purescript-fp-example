module Data.AnimationState where

import Prelude
import Control.Alternative as Alternative
import Data.Maybe as Maybe
import Data.Foldable as Foldable
import Data.TransitionStatus as TransitionStatus

type AnimationState =
  { target :: Number
  , progress :: Number
  }

empty :: AnimationState
empty =
  { target: 0.0
  , progress: 0.0
  }

toTransitionStatus :: AnimationState -> TransitionStatus.TransitionStatus
toTransitionStatus state = Maybe.fromMaybe TransitionStatus.Mounting do
  Foldable.oneOf
    [ do
        Alternative.guard $ state.progress /= state.target && state.target == 1.0
        pure TransitionStatus.Mounting
    , do
        Alternative.guard $ state.progress == state.target && state.target == 1.0
        pure TransitionStatus.Mounted
    , do
        Alternative.guard $ state.progress /= state.target && state.target == 0.0
        pure TransitionStatus.Unmounting
    , do
        Alternative.guard $ state.progress == state.target && state.target == 0.0
        pure TransitionStatus.Unmounted
    ]

done :: AnimationState -> Boolean
done state =
  case toTransitionStatus state of
    TransitionStatus.Mounted -> true
    TransitionStatus.Unmounted -> true
    _ -> false

inProgress :: AnimationState -> Boolean
inProgress state = not $ done state

mounted :: AnimationState -> Boolean
mounted state =
  case toTransitionStatus state of
    TransitionStatus.Mounted -> true
    _ -> false

unmounted :: AnimationState -> Boolean
unmounted state =
  case toTransitionStatus state of
    TransitionStatus.Unmounted -> true
    _ -> false

mounting :: AnimationState -> Boolean
mounting state =
  case toTransitionStatus state of
    TransitionStatus.Mounting -> true
    _ -> false

unmounting :: AnimationState -> Boolean
unmounting state =
  case toTransitionStatus state of
    TransitionStatus.Unmounting -> true
    _ -> false


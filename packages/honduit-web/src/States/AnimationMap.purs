module States.AnimationMap where

import Prelude
import Lib.Math as Math
import Lib.Observable as Observable
import Lib.MergeObservable as MergeObservable
import Lib.SwitchObservable as SwitchObservable
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Tuple.Nested as Tuple
import Data.AnimationState as AnimationState
import Data.AnimationStateAction as AnimationStateAction
import Data.AnimationMapState as AnimationMapState
import Data.AnimationMapStateAction as AnimationMapStateAction
import States.Animation as Animation

reduce :: Observable.Observable Number -> AnimationMapState.AnimationMapState -> Observable.Observable AnimationMapStateAction.AnimationMapStateAction -> Observable.Observable AnimationMapState.AnimationMapState
reduce time initialState actions = Observable.scan
  ( \(key Tuple./\ value) -> Map.insert key value )
  ( initialState )
  ( MergeObservable.toObservable do
      (key Tuple./\ as) <- MergeObservable.fromObservable $ Observable.groupBy (\action -> action.key) actions
      let seed = Maybe.fromMaybe AnimationState.empty $ Map.lookup key initialState
      state <- MergeObservable.fromObservable $ Animation.reduce time seed (as <#> \action -> action.action)
      pure (key Tuple./\ state)
  )

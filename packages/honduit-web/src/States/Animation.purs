module States.Animation where

import Prelude
import Lib.Math as Math
import Lib.Observable as Observable
import Lib.SwitchObservable as SwitchObservable
import Data.AnimationState as AnimationState
import Data.AnimationStateAction as AnimationStateAction

reduce :: Observable.Observable Number -> AnimationState.AnimationState -> Observable.Observable AnimationStateAction.AnimationStateAction -> Observable.Observable AnimationState.AnimationState
reduce time initialState actions = SwitchObservable.toObservable $ SwitchObservable.concat
  [ SwitchObservable.take 1 do
      pure initialState
  , SwitchObservable.switchScan
      ( \action curr -> do
          case action of
            AnimationStateAction.Set next -> pure next
            AnimationStateAction.Tween payload -> do
              progress <- SwitchObservable.call \unsubscribe -> do
                progress <- SwitchObservable.switchScan
                  ( \_ progress -> pure $
                      Math.lerpW
                        progress
                        payload.target
                        payload.speed
                        payload.threshold
                  )
                  ( curr.progress )
                  ( SwitchObservable.fromObservable time )
                case progress /= payload.target of
                  true -> pure progress
                  _ -> unsubscribe payload.target
              pure $ curr { target = payload.target, progress = progress }
            AnimationStateAction.Linear _ -> pure curr
      )
      ( initialState )
      ( SwitchObservable.fromObservable actions )
  ]

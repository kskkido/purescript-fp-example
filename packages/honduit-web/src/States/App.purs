module States.App where

import Prelude
import Lib.RemoteData as RemoteData
import Lib.Observable as Observable
import Lib.MergeObservable as MergeObservable
import Lib.SwitchObservable as SwitchObservable
import Data.Map as Map
import Data.Maybe as Maybe
import Data.AppState as AppState
import Data.AppStateAction as AppStateAction
import Data.AnimationState as AnimationState
import Data.Tuple.Nested as Tuple
import States.Animation as States.Animation
import States.AnimationMap as States.AnimationMap

reduce :: Observable.Observable Number -> AppState.AppState -> Observable.Observable AppStateAction.AppStateAction -> Observable.Observable AppState.AppState
reduce time initialState actions = SwitchObservable.toObservable do
  ( location
    Tuple./\ animationMap
    Tuple./\ homeRequest
    Tuple./\ postsRequest
    Tuple./\ loginRequest
    Tuple./\ loginFormEmail
    Tuple./\ loginFormPassword
    Tuple./\ registerRequest
    Tuple./\ registerFormUsername
    Tuple./\ registerFormEmail
    Tuple./\ registerFormPassword
    Tuple./\ _
  ) <-
    ( SwitchObservable.distinctUntilChanged do
        SwitchObservable.concat
          [ do
              action <- SwitchObservable.fromObservable actions
              case action of
                AppStateAction.SetLocation x -> pure x
                _ -> SwitchObservable.never
          , do
              pure initialState.location
          ]
    ) SwitchObservable./\
    ( SwitchObservable.fromObservable $
        States.AnimationMap.reduce
          time
          initialState.animationMap
          ( SwitchObservable.toObservable do
              action <- SwitchObservable.fromObservable actions
              case action of
                AppStateAction.SetAnimation x -> pure x
                _ -> SwitchObservable.never
          )
    ) SwitchObservable./\
    ( SwitchObservable.distinctWhile RemoteData.equalStatus do
        SwitchObservable.concat
          [ do
              action <- SwitchObservable.fromObservable actions
              case action of
                AppStateAction.SetHomeRequest x -> pure x
                _ -> SwitchObservable.never
          , do
              pure initialState.pages.home.request
          ]
    ) SwitchObservable./\
    ( SwitchObservable.distinctWhile RemoteData.equalStatus do
        SwitchObservable.concat
          [ do
              action <- SwitchObservable.fromObservable actions
              case action of
                AppStateAction.SetPostsRequest x -> pure x
                _ -> SwitchObservable.never
          , do
              pure initialState.pages.posts.request
          ]
    ) SwitchObservable./\
    ( SwitchObservable.distinctWhile RemoteData.equalStatus do
        SwitchObservable.concat
          [ do
              action <- SwitchObservable.fromObservable actions
              case action of
                AppStateAction.SetLoginRequest x -> pure x
                _ -> SwitchObservable.never
          , do
              pure initialState.pages.login.request
          ]
    ) SwitchObservable./\
    ( SwitchObservable.distinctUntilChanged do
        SwitchObservable.concat
          [ do
              action <- SwitchObservable.fromObservable actions
              case action of
                AppStateAction.SetLoginFormEmail x -> pure x
                _ -> SwitchObservable.never
          , do
              pure initialState.pages.login.form.email
          ]
    ) SwitchObservable./\
    ( SwitchObservable.distinctUntilChanged do
        SwitchObservable.concat
          [ do
              action <- SwitchObservable.fromObservable actions
              case action of
                AppStateAction.SetLoginFormPassword x -> pure x
                _ -> SwitchObservable.never
          , do
              pure initialState.pages.login.form.password
          ]
    ) SwitchObservable./\
    ( SwitchObservable.distinctWhile RemoteData.equalStatus do
        SwitchObservable.concat
          [ do
              action <- SwitchObservable.fromObservable actions
              case action of
                AppStateAction.SetRegisterRequest x -> pure x
                _ -> SwitchObservable.never
          , do
              pure initialState.pages.register.request
          ]
    ) SwitchObservable./\
    ( SwitchObservable.distinctUntilChanged do
        SwitchObservable.concat
          [ do
              action <- SwitchObservable.fromObservable actions
              case action of
                AppStateAction.SetRegisterFormUsername x -> pure x
                _ -> SwitchObservable.never
          , do
              pure initialState.pages.register.form.username
          ]
    ) SwitchObservable./\
    ( SwitchObservable.distinctUntilChanged do
        SwitchObservable.concat
          [ do
              action <- SwitchObservable.fromObservable actions
              case action of
                AppStateAction.SetRegisterFormEmail x -> pure x
                _ -> SwitchObservable.never
          , do
              pure initialState.pages.register.form.email
          ]
    ) SwitchObservable./\
    ( SwitchObservable.distinctUntilChanged do
        SwitchObservable.concat
          [ do
              action <- SwitchObservable.fromObservable actions
              case action of
                AppStateAction.SetRegisterFormPassword x -> pure x
                _ -> SwitchObservable.never
          , do
              pure initialState.pages.register.form.password
          ]
    ) SwitchObservable./\
    ( pure unit )
  pure $
    { location: location
    , animationMap: animationMap
    , pages:
      { home:
        { request: homeRequest
        }
      , posts:
        { request: postsRequest
        }
      , login:
        { request: loginRequest
        , form:
          { email: loginFormEmail
          , password: loginFormPassword
          }
        }
      , register:
        { request: registerRequest
        , form:
          { username: registerFormUsername
          , email: registerFormEmail
          , password: registerFormPassword
          }
        }
      }
    }

getAnimation :: String -> Observable.Observable AppState.AppState -> Observable.Observable AnimationState.AnimationState
getAnimation key states = SwitchObservable.toObservable do
  SwitchObservable.distinctUntilChanged do
    state <- SwitchObservable.fromObservable states
    case Map.lookup key state.animationMap of
      Maybe.Just animation -> pure animation
      _ -> pure AnimationState.empty

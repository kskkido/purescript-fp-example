module Controllers.App where

import Prelude
import Effect.Aff as Effect.Aff
import Effect.Class as Effect.Class
import Effect.Console as Effect.Console
import Control.Monad.Reader as Reader
import Control.Monad.Trans.Class as Trans
import Network.RemoteData as RemoteData
import Lib.Plus as Plus
import Lib.Observable as Observable
import Lib.SwitchObservable as SwitchObservable
import Data.AppViewEvent as AppViewEvent
import Data.AppMainContext as AppMainContext
import Data.List as List
import Data.Time.Duration as Time.Duration
import Data.Validator as Validator
import Data.AppLocation as AppLocation
import Data.RouteToken as RouteToken
import Data.Path as Path
import Data.PathValidator as PathValidator
import Data.AppState as AppState
import Data.AppStateAction as AppStateAction
import Data.AnimationStateAction as AnimationStateAction
import Views.App as Views.App
import Stores.App as Stores.App

type Controller = Reader.ReaderT AppMainContext.AppMainContext

main :: forall m. Effect.Class.MonadEffect m => AppState.AppState -> Controller m Unit
main state = do
  context <- Reader.ask
  store <- Stores.App.fromState context.time state
  events <- Views.App.render store.state
  actions <- handleEvents events store.state
  void $ Effect.Class.liftEffect do
    Observable.subscribe actions \action -> do
      Effect.Aff.launchAff_ do
        Effect.Aff.delay (Time.Duration.Milliseconds 0.0)
        Effect.Class.liftEffect do
          store.dispatch action

handleEvents :: forall m. Effect.Class.MonadEffect m => Observable.Observable AppViewEvent.AppViewEvent -> Observable.Observable AppState.AppState -> Controller m (Observable.Observable AppStateAction.AppStateAction)
handleEvents events states = do
  context <- Reader.ask
  Trans.lift $ pure $
    SwitchObservable.toObservable $ SwitchObservable.concat
      [ do
          event <- SwitchObservable.fromObservable events
          Effect.Class.liftEffect do
            case event of
              AppViewEvent.Noop -> pure unit
              _ -> Effect.Console.log ("event: " <> show event)
          case event of
            AppViewEvent.SetPathString pathname -> do
              location <- pure $ context.pathValidator pathname
              pure $ AppStateAction.SetLocation location
            AppViewEvent.PreloadHome -> do
              state <- SwitchObservable.fromObservable states
              case state.pages.home.request of
                RemoteData.Loading -> do
                  pure AppStateAction.Noop
                _ -> do
                  SwitchObservable.concat
                    [ do
                        pure RemoteData.Loading <#> AppStateAction.SetHomeRequest
                    , do
                        request <- SwitchObservable.fromObservable $ Observable.fromAff context.appApiClient.getHome
                        pure $ AppStateAction.SetHomeRequest request
                    ]
            AppViewEvent.MountHome -> do
              SwitchObservable.concat
                [ do
                    let tween = AnimationStateAction.Tween { target: 1.0, speed: 0.1, threshold: 0.001 }
                    pure $ AppStateAction.SetAnimation { key: show AppLocation.Home, action: tween }
                ]
            AppViewEvent.UnmountHome -> do
              SwitchObservable.concat
                [ do
                    pure RemoteData.NotAsked <#> AppStateAction.SetHomeRequest
                , do
                    let tween = AnimationStateAction.Tween { target: 0.0, speed: 0.1, threshold: 0.01 }
                    pure $ AppStateAction.SetAnimation { key: show AppLocation.Home, action: tween }
                ]
            AppViewEvent.PreloadPosts -> do
              state <- SwitchObservable.fromObservable states
              case state.pages.posts.request of
                RemoteData.Loading -> do
                  pure AppStateAction.Noop
                _ -> do
                  SwitchObservable.concat
                    [ do
                        pure RemoteData.Loading <#> AppStateAction.SetPostsRequest
                    , do
                        request <- SwitchObservable.fromObservable $ Observable.fromAff context.appApiClient.getPosts
                        pure $ AppStateAction.SetPostsRequest request
                    ]
            AppViewEvent.MountPosts -> do
              SwitchObservable.concat
                [ do
                    let tween = AnimationStateAction.Tween { target: 1.0, speed: 0.1, threshold: 0.001 }
                    pure $ AppStateAction.SetAnimation { key: show AppLocation.Posts, action: tween }
                ]
            AppViewEvent.UnmountPosts -> do
              SwitchObservable.concat
                [ do
                    pure RemoteData.NotAsked <#> AppStateAction.SetPostsRequest
                , do
                    let tween = AnimationStateAction.Tween { target: 0.0, speed: 0.1, threshold: 0.01 }
                    pure $ AppStateAction.SetAnimation { key: show AppLocation.Posts, action: tween }
                ]
            AppViewEvent.PreloadLogin -> do
              state <- SwitchObservable.fromObservable states
              case state.pages.login.request of
                RemoteData.Loading -> do
                  pure AppStateAction.Noop
                _ -> do
                  SwitchObservable.concat
                    [ do
                        pure RemoteData.Loading <#> AppStateAction.SetLoginRequest
                    , do
                        request <- SwitchObservable.fromObservable $ Observable.fromAff context.appApiClient.getLogin
                        pure $ AppStateAction.SetLoginRequest request
                    ]
            AppViewEvent.MountLogin -> do
              SwitchObservable.concat
                [ do
                    let tween = AnimationStateAction.Tween { target: 1.0, speed: 0.1, threshold: 0.001 }
                    pure $ AppStateAction.SetAnimation { key: show AppLocation.Login, action: tween }
                ]
            AppViewEvent.UnmountLogin -> do
              SwitchObservable.concat
                [ do
                    pure RemoteData.NotAsked <#> AppStateAction.SetLoginRequest
                , do
                    let tween = AnimationStateAction.Tween { target: 0.0, speed: 0.1, threshold: 0.01 }
                    pure $ AppStateAction.SetAnimation { key: show AppLocation.Login, action: tween }
                ]
            AppViewEvent.SetLoginFormEmail value -> do
              SwitchObservable.concat
                [ do
                    pure $ AppStateAction.SetLoginFormEmail value
                ]
            AppViewEvent.SetLoginFormPassword value -> do
              SwitchObservable.concat
                [ do
                    pure $ AppStateAction.SetLoginFormPassword value
                ]
            AppViewEvent.PreloadRegister -> do
              state <- SwitchObservable.fromObservable states
              case state.pages.register.request of
                RemoteData.Loading -> do
                  pure AppStateAction.Noop
                _ -> do
                  SwitchObservable.concat
                    [ do
                        pure RemoteData.Loading <#> AppStateAction.SetRegisterRequest
                    , do
                        request <- SwitchObservable.fromObservable $ Observable.fromAff context.appApiClient.getRegister
                        pure $ AppStateAction.SetRegisterRequest request
                    ]
            AppViewEvent.MountRegister -> do
              SwitchObservable.concat
                [ do
                    let tween = AnimationStateAction.Tween { target: 1.0, speed: 0.1, threshold: 0.001 }
                    pure $ AppStateAction.SetAnimation { key: show AppLocation.Register, action: tween }
                ]
            AppViewEvent.UnmountRegister -> do
              SwitchObservable.concat
                [ do
                    pure RemoteData.NotAsked <#> AppStateAction.SetRegisterRequest
                , do
                    let tween = AnimationStateAction.Tween { target: 0.0, speed: 0.1, threshold: 0.01 }
                    pure $ AppStateAction.SetAnimation { key: show AppLocation.Register, action: tween }
                ]
            AppViewEvent.SetRegisterFormUsername value -> do
              SwitchObservable.concat
                [ do
                    pure $ AppStateAction.SetRegisterFormUsername value
                ]
            AppViewEvent.SetRegisterFormEmail value -> do
              SwitchObservable.concat
                [ do
                    pure $ AppStateAction.SetRegisterFormEmail value
                ]
            AppViewEvent.SetRegisterFormPassword value -> do
              SwitchObservable.concat
                [ do
                    pure $ AppStateAction.SetRegisterFormPassword value
                ]
            _ -> pure AppStateAction.Noop
      ]


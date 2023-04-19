module Views.App where

import Prelude
import Effect.Class as Effect.Class
import Effect.Console as Effect.Console
import Control.Alternative as Alternative
import Control.Monad.Trans.Class as Trans
import Control.Monad.Reader as Reader
import Control.Monad.Maybe.Trans as MaybeT
import Network.RemoteData as RemoteData
import Web.Event.Event as Web.Event.Event
import Web.DOM.Element as Web.DOM.Element
import Web.HTML.Window as Web.HTML.Window
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Lib.RemoteData as RemoteData
import Lib.Observable as Observable
import Lib.MergeObservable as MergeObservable
import Lib.SwitchObservable as SwitchObservable
import Lib.Dom.HtmlElement as HtmlElement
import Lib.Dom.Element as Element
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Foldable as Foldable
import Data.AnimationState as AnimationState
import Data.TransitionStatus as TransitionStatus
import Data.HistoryStore as HistoryStore
import Data.AppState as AppState
import Data.AppViewEvent as AppViewEvent
import Data.AppLocation as AppLocation
import Data.AppMainContext as AppMainContext
import Components.Page as Components.Page
import Components.Link as Components.Link
import Components.LoginForm as Components.LoginForm
import Components.RegisterForm as Components.RegisterForm
import States.App as States.App

type App = Reader.ReaderT AppMainContext.AppMainContext

render :: forall m. Effect.Class.MonadEffect m => Observable.Observable AppState.AppState -> App m (Observable.Observable AppViewEvent.AppViewEvent)
render states = do
  context <- Reader.ask
  Effect.Class.liftEffect do
    pure $ SwitchObservable.toObservable $ SwitchObservable.concat
      [ do
          history <- SwitchObservable.fromObservable context.historyStore.state
          pure $ AppViewEvent.SetPathString history.pathname
      , do
          _ <- SwitchObservable.fromObservable $ Components.Link.observe context
          pure AppViewEvent.Noop
      , do
          animation <- do
            SwitchObservable.concat $
              [ show AppLocation.Home
              , show AppLocation.Posts
              , show AppLocation.Login
              , show AppLocation.Register
              ] <#>
              \key -> do
                SwitchObservable.fromObservable $ States.App.getAnimation key states
          Effect.Class.liftEffect do
            flip Reader.runReader animation.progress do
              Components.Page.animate context.page
            case AnimationState.toTransitionStatus animation of
              TransitionStatus.Unmounted -> do
                HtmlElement.removeChildren context.page
              _ -> pure unit
          pure AppViewEvent.Noop
      , do
          match <- SwitchObservable.distinctUntilChanged do
            state <- SwitchObservable.fromObservable states
            pure $ state.location == AppLocation.Home
          SwitchObservable.take 1 do
            let key = show AppLocation.Home
            animation <- SwitchObservable.fromObservable $ States.App.getAnimation key states
            case match of
              true -> do
                case AnimationState.toTransitionStatus animation of
                  TransitionStatus.Unmounted -> pure AppViewEvent.PreloadHome
                  TransitionStatus.Unmounting -> pure AppViewEvent.MountHome
                  _ -> SwitchObservable.never
              _ -> do
                case AnimationState.toTransitionStatus animation of
                  TransitionStatus.Mounted -> pure AppViewEvent.UnmountHome
                  TransitionStatus.Mounting -> pure AppViewEvent.UnmountHome
                  _ -> SwitchObservable.never
      , do
          request <- SwitchObservable.distinctWhile RemoteData.equalStatus do
            state <- SwitchObservable.fromObservable states
            pure $ state.pages.home.request
          location <- SwitchObservable.distinctUntilChanged do
            state <- SwitchObservable.fromObservable states
            pure $ state.location
          SwitchObservable.until $
            [ show AppLocation.Home
            , show AppLocation.Posts
            , show AppLocation.Login
            , show AppLocation.Register
            ] <#>
            \key -> do
              animation <- SwitchObservable.fromObservable $ States.App.getAnimation key states
              Alternative.guard (AnimationState.unmounted animation)
          Alternative.guard (location == AppLocation.Home)
          case request of
            RemoteData.Success content -> do
              Effect.Class.liftEffect do
                HtmlElement.appendHtmlElement content context.page
              pure AppViewEvent.MountHome
            _ -> do
              pure AppViewEvent.Noop
      , do
          match <- SwitchObservable.distinctUntilChanged do
            state <- SwitchObservable.fromObservable states
            pure $ state.location == AppLocation.Posts
          SwitchObservable.take 1 do
            let key = show AppLocation.Posts
            animation <- SwitchObservable.fromObservable $ States.App.getAnimation key states
            case match of
              true -> do
                case AnimationState.toTransitionStatus animation of
                  TransitionStatus.Unmounted -> pure AppViewEvent.PreloadPosts
                  TransitionStatus.Unmounting -> pure AppViewEvent.MountPosts
                  _ -> SwitchObservable.never
              _ -> do
                case AnimationState.toTransitionStatus animation of
                  TransitionStatus.Mounted -> pure AppViewEvent.UnmountPosts
                  TransitionStatus.Mounting -> pure AppViewEvent.UnmountPosts
                  _ -> SwitchObservable.never
      , do
          request <- SwitchObservable.distinctWhile RemoteData.equalStatus do
            state <- SwitchObservable.fromObservable states
            pure $ state.pages.posts.request
          location <- SwitchObservable.distinctUntilChanged do
            state <- SwitchObservable.fromObservable states
            pure $ state.location
          SwitchObservable.until $
            [ show AppLocation.Home
            , show AppLocation.Posts
            , show AppLocation.Login
            , show AppLocation.Register
            ] <#>
            \key -> do
              animation <- SwitchObservable.fromObservable $ States.App.getAnimation key states
              Alternative.guard (AnimationState.unmounted animation)
          Alternative.guard (location == AppLocation.Posts)
          case request of
            RemoteData.Success content -> do
              Effect.Class.liftEffect do
                HtmlElement.appendHtmlElement content context.page
              SwitchObservable.concat
                [ pure AppViewEvent.MountPosts
                ]
            _ -> do
              pure AppViewEvent.Noop
      , do
          match <- SwitchObservable.distinctUntilChanged do
            state <- SwitchObservable.fromObservable states
            pure $ state.location == AppLocation.Login
          SwitchObservable.take 1 do
            let key = show AppLocation.Login
            animation <- SwitchObservable.fromObservable $ States.App.getAnimation key states
            case match of
              true -> do
                case AnimationState.toTransitionStatus animation of
                  TransitionStatus.Unmounted -> pure AppViewEvent.PreloadLogin
                  TransitionStatus.Unmounting -> pure AppViewEvent.MountLogin
                  _ -> SwitchObservable.never
              _ -> do
                case AnimationState.toTransitionStatus animation of
                  TransitionStatus.Mounted -> pure AppViewEvent.UnmountLogin
                  TransitionStatus.Mounting -> pure AppViewEvent.UnmountLogin
                  _ -> SwitchObservable.never
      , do
          request <- SwitchObservable.distinctWhile RemoteData.equalStatus do
            state <- SwitchObservable.fromObservable states
            pure $ state.pages.login.request
          location <- SwitchObservable.distinctUntilChanged do
            state <- SwitchObservable.fromObservable states
            pure $ state.location
          SwitchObservable.until $
            [ show AppLocation.Home
            , show AppLocation.Posts
            , show AppLocation.Login
            , show AppLocation.Register
            ] <#>
            \key -> do
              animation <- SwitchObservable.fromObservable $ States.App.getAnimation key states
              Alternative.guard (AnimationState.unmounted animation)
          Alternative.guard (location == AppLocation.Login)
          case request of
            RemoteData.Success content -> do
              Effect.Class.liftEffect do
                HtmlElement.appendHtmlElement content context.page
              SwitchObservable.concat
                [ SwitchObservable.fromObservable $ Components.LoginForm.observe states context
                , pure AppViewEvent.MountLogin
                ]
            _ -> do
              pure AppViewEvent.Noop
      , do
          match <- SwitchObservable.distinctUntilChanged do
            state <- SwitchObservable.fromObservable states
            pure $ state.location == AppLocation.Register
          SwitchObservable.take 1 do
            let key = show AppLocation.Register
            animation <- SwitchObservable.fromObservable $ States.App.getAnimation key states
            case match of
              true -> do
                case AnimationState.toTransitionStatus animation of
                  TransitionStatus.Unmounted -> pure AppViewEvent.PreloadRegister
                  TransitionStatus.Unmounting -> pure AppViewEvent.MountRegister
                  _ -> SwitchObservable.never
              _ -> do
                case AnimationState.toTransitionStatus animation of
                  TransitionStatus.Mounted -> pure AppViewEvent.UnmountRegister
                  TransitionStatus.Mounting -> pure AppViewEvent.UnmountRegister
                  _ -> SwitchObservable.never
      , do
          request <- SwitchObservable.distinctWhile RemoteData.equalStatus do
            state <- SwitchObservable.fromObservable states
            pure $ state.pages.register.request
          location <- SwitchObservable.distinctUntilChanged do
            state <- SwitchObservable.fromObservable states
            pure $ state.location
          SwitchObservable.until $
            [ show AppLocation.Home
            , show AppLocation.Posts
            , show AppLocation.Login
            , show AppLocation.Register
            ] <#>
            \key -> do
              animation <- SwitchObservable.fromObservable $ States.App.getAnimation key states
              Alternative.guard (AnimationState.unmounted animation)
          Alternative.guard (location == AppLocation.Register)
          case request of
            RemoteData.Success content -> do
              Effect.Class.liftEffect do
                HtmlElement.appendHtmlElement content context.page
              SwitchObservable.concat
                [ SwitchObservable.fromObservable $ Components.RegisterForm.observe states context
                , pure AppViewEvent.MountRegister
                ]
            _ -> do
              pure AppViewEvent.Noop
      ]


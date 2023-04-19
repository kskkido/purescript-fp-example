module Components.RegisterForm where

import Prelude
import Effect.Class as Effect.Class
import Control.Monad.Trans.Class as Trans
import Control.Monad.Reader as Reader
import Control.Monad.Maybe.Trans as MaybeT
import Web.Event.Event as Web.Event.Event
import Web.DOM.Element as Web.DOM.Element
import Web.HTML.Window as Web.HTML.Window
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Web.HTML.HTMLInputElement as Web.HTML.HTMLInputElement
import Lib.Dom.HtmlElement as HtmlElement
import Lib.Dom.Element as Element
import Lib.Observable as Observable
import Lib.SwitchObservable as SwitchObservable
import Data.Foldable as Foldable
import Data.AppMainContext as AppMainContext
import Data.AppViewEvent as AppViewEvent
import Data.AppState as AppState
import Data.HistoryStore as HistoryStore

observe :: Observable.Observable AppState.AppState -> AppMainContext.AppMainContext -> Observable.Observable AppViewEvent.AppViewEvent
observe states context = SwitchObservable.toObservable do
  document <- Effect.Class.liftEffect do
    Web.HTML.Window.document context.window
  form <- SwitchObservable.fromMaybeIO do
    MaybeT.MaybeT do
      let parent = Web.HTML.HTMLDocument.toParentNode document
      HtmlElement.fromQueryMatch "#register_form" parent
  username <- SwitchObservable.fromMaybeIO do
    let parent = Web.HTML.HTMLElement.toParentNode form
    element <- MaybeT.MaybeT $ HtmlElement.fromQueryMatch "#username" parent
    MaybeT.MaybeT $ pure $ Web.HTML.HTMLInputElement.fromHTMLElement element
  email <- SwitchObservable.fromMaybeIO do
    let parent = Web.HTML.HTMLElement.toParentNode form
    element <- MaybeT.MaybeT $ HtmlElement.fromQueryMatch "#email" parent
    MaybeT.MaybeT $ pure $ Web.HTML.HTMLInputElement.fromHTMLElement element
  password <- SwitchObservable.fromMaybeIO do
    let parent = Web.HTML.HTMLElement.toParentNode form
    element <- MaybeT.MaybeT $ HtmlElement.fromQueryMatch "#password" parent
    MaybeT.MaybeT $ pure $ Web.HTML.HTMLInputElement.fromHTMLElement element
  state <- SwitchObservable.take 1 do
    state <- SwitchObservable.fromObservable states
    pure state.pages.register.form
  Effect.Class.liftEffect do
    Web.HTML.HTMLInputElement.setValue state.username username
    Web.HTML.HTMLInputElement.setValue state.email email
    Web.HTML.HTMLInputElement.setValue state.password password
  SwitchObservable.concat
    [ do
        event <- SwitchObservable.fromEvent
          ( Web.Event.Event.EventType "submit" )
          ( Web.HTML.HTMLElement.toEventTarget form )
        Effect.Class.liftEffect do
          Web.Event.Event.preventDefault event
          pure $ AppViewEvent.SubmitRegisterForm
    , do
        _ <- SwitchObservable.fromEvent
          ( Web.Event.Event.EventType "change" )
          ( Web.HTML.HTMLInputElement.toEventTarget username )
        Effect.Class.liftEffect do
          value <- Web.HTML.HTMLInputElement.value username
          pure $ AppViewEvent.SetRegisterFormUsername value
    , do
        _ <- SwitchObservable.fromEvent
          ( Web.Event.Event.EventType "change" )
          ( Web.HTML.HTMLInputElement.toEventTarget email )
        Effect.Class.liftEffect do
          value <- Web.HTML.HTMLInputElement.value email
          pure $ AppViewEvent.SetRegisterFormEmail value
    , do
        _ <- SwitchObservable.fromEvent
          ( Web.Event.Event.EventType "change" )
          ( Web.HTML.HTMLInputElement.toEventTarget password )
        Effect.Class.liftEffect do
          value <- Web.HTML.HTMLInputElement.value password
          pure $ AppViewEvent.SetRegisterFormPassword value
    ]

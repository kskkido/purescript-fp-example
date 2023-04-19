module Components.Link where

import Prelude
import Effect.Class as Effect.Class
import Control.Monad.Trans.Class as Trans
import Control.Monad.Reader as Reader
import Control.Monad.Maybe.Trans as MaybeT
import Web.Event.Event as Web.Event.Event
import Web.DOM.Element as Web.DOM.Element
import Web.HTML.Window as Web.HTML.Window
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Lib.Dom.HtmlElement as HtmlElement
import Lib.Dom.Element as Element
import Lib.Observable as Observable
import Lib.SwitchObservable as SwitchObservable
import Data.Foldable as Foldable
import Data.AppMainContext as AppMainContext
import Data.HistoryStore as HistoryStore

observe :: AppMainContext.AppMainContext -> Observable.Observable Unit
observe context = do
  elements <- Effect.Class.liftEffect do
    document <- do
      Web.HTML.Window.document context.window
    let parent = Web.HTML.HTMLDocument.toParentNode document
    Element.fromQueryMatches "[data-component-type=\"link\"]" parent
  SwitchObservable.toObservable do
    flip Foldable.foldMap elements $ \element -> do
      event <- SwitchObservable.fromEvent
        ( Web.Event.Event.EventType "click" )
        ( Web.DOM.Element.toEventTarget element )
      Effect.Class.liftEffect do
        Web.Event.Event.preventDefault event
        void $ MaybeT.runMaybeT do
          pathname <- MaybeT.MaybeT $ Web.DOM.Element.getAttribute "data-path" element
          Effect.Class.liftEffect do
            let action = HistoryStore.Push pathname
            context.historyStore.dispatch action

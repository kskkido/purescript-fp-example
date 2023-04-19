module Stores.History where

import Prelude
import Foreign as Foreign
import Effect as Effect
import Effect.Class as Effect.Class
import Effect.Console as Effect.Console
import Web.HTML as Web.HTML
import Web.HTML.Window as Web.HTML.Window
import Web.HTML.History as Web.HTML.History
import Web.HTML.Location as Web.HTML.Location
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Web.HTML.Event.PopStateEvent as Web.HTML.Event.PopStateEvent
import Web.Event.Event as Web.Event.Event
import Web.Event.EventTarget as Web.Event.EventTarget
import Lib.Observable as Observable
import Lib.SwitchObservable as SwitchObservable
import Lib.BehaviorSubject as BehaviorSubject
import Data.HistoryState as HistoryState
import Data.HistoryStateAction as HistoryStateAction
import States.History as States.History

type HistoryStore =
  { state :: Observable.Observable HistoryState.HistoryState
  , dispatch :: HistoryStateAction.HistoryStateAction -> Effect.Effect Unit
  }

fromWindow :: forall m. Effect.Class.MonadEffect m => Web.HTML.Window -> m HistoryStore
fromWindow window = do
  Effect.Class.liftEffect do
    history <- Web.HTML.Window.history window
    location <- Web.HTML.Window.location window
    pathname <- Web.HTML.Location.pathname location
    let initialState = { pathname: pathname }
    subject <- BehaviorSubject.from HistoryStateAction.Noop
    do
      let eventTarget = Web.HTML.Window.toEventTarget window
          event = Web.Event.Event.EventType "popstate"
      void $ flip Observable.subscribe subject.notify $
        ( ( Observable.fromEvent event eventTarget ) #
          ( Observable.filterMap Web.HTML.Event.PopStateEvent.fromEvent ) #
          ( Observable.bindIO $ \_ -> Web.HTML.Location.pathname location ) #
          ( map $ \pathname -> HistoryStateAction.Push pathname )
        )
    state <- Observable.singleton
      ( ( Observable.from subject.listen ) #
        ( States.History.reduce initialState )
      )
    pure $
      { state: state
      , dispatch: \action -> do
          case action of
            HistoryStateAction.Push pathname -> do
              let title = Web.HTML.History.DocumentTitle ""
                  url = Web.HTML.History.URL pathname
              Web.HTML.History.pushState (Foreign.unsafeToForeign {}) title url history
            HistoryStateAction.Noop -> pure unit
          subject.notify action
      }

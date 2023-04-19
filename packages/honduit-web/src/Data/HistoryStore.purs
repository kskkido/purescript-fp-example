module Data.HistoryStore where

import Prelude
import Effect as Effect
import Effect.Console as Effect.Console
import Foreign as Foreign
import Web.HTML as Web.HTML
import Web.HTML.Window as Web.HTML.Window
import Web.HTML.Location as Web.HTML.Location
import Web.HTML.History as Web.HTML.History
import Web.Event.Event as Web.Event.Event
import Web.Event.EventTarget as Web.Event.EventTarget
import Web.HTML.Event.PopStateEvent as Web.HTML.Event.PopStateEvent
import Lib.BehaviorSubject as BehaviorSubject
import Lib.Observable as Observable
import Data.HistoryState as HistoryState

type HistoryStore =
  { state :: Observable.Observable HistoryState.HistoryState
  , dispatch :: HistoryStoreAction -> Effect.Effect Unit
  }

data HistoryStoreAction =
    Push String
  | Noop

fromWindow :: Web.HTML.Window -> Effect.Effect HistoryStore
fromWindow window = do
  history <- Web.HTML.Window.history window
  location <- Web.HTML.Window.location window
  pathname <- Web.HTML.Location.pathname location
  let initialState = { pathname: pathname }
  subject <- BehaviorSubject.from Noop
  do
    let eventTarget = Web.HTML.Window.toEventTarget window
        event = Web.Event.Event.EventType "popstate"
    void $ flip Observable.subscribe subject.notify $
      ( ( Observable.fromEvent event eventTarget ) #
        ( Observable.filterMap Web.HTML.Event.PopStateEvent.fromEvent ) #
        ( Observable.bindIO $ \_ -> Web.HTML.Location.pathname location ) #
        ( map $ \pathname -> Push pathname )
      )
  pure $
    { state: (
        ( Observable.from subject.listen ) #
        ( Observable.switchScan reduce initialState )
      )
    , dispatch: \action -> do
        case action of
          Push pathname -> do
            let title = Web.HTML.History.DocumentTitle ""
                url = Web.HTML.History.URL pathname
            Web.HTML.History.pushState (Foreign.unsafeToForeign {}) title url history
          Noop -> pure unit
        subject.notify action
    }

reduce :: HistoryStoreAction -> HistoryState.HistoryState -> Observable.Observable HistoryState.HistoryState
reduce action curr =
  pure $ case action of
    Push pathname -> curr { pathname = pathname }
    Noop -> curr

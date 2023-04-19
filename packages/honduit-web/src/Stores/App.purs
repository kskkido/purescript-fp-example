module Stores.App where

import Prelude
import Effect as Effect
import Effect.Class as Effect.Class
import Effect.Console as Effect.Console
import Network.RemoteData as RemoteData
import Lib.Subject as Subject
import Lib.Observable as Observable
import Data.AppLocation as AppLocation
import Data.AppState as AppState
import Data.AppStateAction as AppStateAction
import States.App as States.App

type AppStore =
  { state :: Observable.Observable AppState.AppState
  , dispatch :: AppStateAction.AppStateAction -> Effect.Effect Unit
  }

fromState :: forall m. Effect.Class.MonadEffect m => Observable.Observable Number -> AppState.AppState -> m AppStore
fromState time seed = do
  Effect.Class.liftEffect do
    subject <- Subject.from
    state <- Observable.singleton
      ( ( Observable.from subject.listen ) #
        ( States.App.reduce time seed )
      )
    pure $
      { state: state
      , dispatch: \action -> do
          case action of
            AppStateAction.Noop -> pure unit
            _ -> Effect.Console.log ("action: " <> show action)
          subject.notify action
      }


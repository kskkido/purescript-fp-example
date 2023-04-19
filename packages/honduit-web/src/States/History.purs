module States.History where

import Prelude
import Lib.Observable as Observable
import Lib.SwitchObservable as SwitchObservable
import Data.HistoryState as HistoryState
import Data.HistoryStateAction as HistoryStateAction

reduce :: HistoryState.HistoryState -> Observable.Observable HistoryStateAction.HistoryStateAction -> Observable.Observable HistoryState.HistoryState
reduce initialState actions = SwitchObservable.toObservable $ SwitchObservable.concat
  [ SwitchObservable.switchScan
      ( \action curr -> do
          pure $ case action of
            HistoryStateAction.Push pathname -> curr { pathname = pathname }
            HistoryStateAction.Noop -> curr
      )
      ( initialState )
      ( SwitchObservable.fromObservable actions )
  ]

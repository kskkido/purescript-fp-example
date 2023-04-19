module Data.AppState where

import Affjax as Affjax
import Network.RemoteData as RemoteData
import Lib.Dom.HtmlElement as HtmlElement
import Data.Map as Map
import Data.Maybe as Maybe
import Data.AppLocation as AppLocation
import Data.AnimationState as AnimationState
import Data.AnimationMapState as AnimationMapState
import Data.LoginFormState as LoginFormState
import Data.RegisterFormState as RegisterFormState

type AppState =
  { location :: AppLocation.AppLocation
  , animationMap :: AnimationMapState.AnimationMapState
  , pages ::
    { home ::
      { request :: RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement
      }
    , posts ::
      { request :: RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement
      }
    , login ::
      { request :: RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement
      , form :: LoginFormState.LoginFormState
      }
    , register ::
      { request :: RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement
      , form :: RegisterFormState.RegisterFormState
      }
    }
  }


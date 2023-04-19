module Data.AppApiClientInterface where

import Prelude
import Effect as Effect
import Effect.Aff as Effect.Aff
import Affjax as Affjax
import Network.RemoteData as RemoteData
import Lib.Dom.HtmlElement as HtmlElement

type AppApiClientInterface =
  { getHome :: Effect.Aff.Aff (RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement)
  , getPosts :: Effect.Aff.Aff (RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement)
  , getLogin :: Effect.Aff.Aff (RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement)
  , getRegister :: Effect.Aff.Aff (RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement)
  , get404 :: Effect.Aff.Aff (RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement)
  }

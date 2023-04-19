module Data.AppStateAction where

import Prelude
import Affjax as Affjax
import Network.RemoteData as RemoteData
import Lib.RemoteData as RemoteData
import Lib.Dom.HtmlElement as HtmlElement
import Data.Generic.Rep as Generic
import Data.AppLocation as AppLocation
import Data.AnimationStateAction as AnimationStateAction
import Data.AnimationMapStateAction as AnimationMapStateAction

data AppStateAction =
    SetLocation AppLocation.AppLocation
  | SetAnimation AnimationMapStateAction.AnimationMapStateAction
  | SetHomeRequest (RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement)
  | SetPostsRequest (RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement)
  | SetLoginRequest (RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement)
  | SetLoginFormEmail String
  | SetLoginFormPassword String
  | SetRegisterRequest (RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement)
  | SetRegisterFormUsername String
  | SetRegisterFormEmail String
  | SetRegisterFormPassword String
  | Noop

derive instance appStateActionGeneric :: Generic.Generic AppStateAction _
instance appStateActionShow :: Show AppStateAction where
  show = case _ of
    SetLocation x -> "SetLocation " <> show x
    SetAnimation _ -> "SetAnimation"
    SetHomeRequest x -> "SetHomeRequest " <> RemoteData.showStatus x
    SetPostsRequest x -> "SetPostsRequest " <> RemoteData.showStatus x
    SetLoginRequest x -> "SetLoginRequest " <> RemoteData.showStatus x
    SetRegisterRequest x -> "SetRegisterRequest " <> RemoteData.showStatus x
    _ -> "Noop"


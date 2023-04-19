module Data.AppViewEvent where

import Prelude
import Data.AppLocation as AppLocation

data AppViewEvent =
    SetPathString String
  | PreloadHome
  | MountHome
  | UnmountHome
  | PreloadPosts
  | MountPosts
  | UnmountPosts
  | PreloadLogin
  | MountLogin
  | UnmountLogin
  | SubmitLoginForm
  | SetLoginFormEmail String
  | SetLoginFormPassword String
  | PreloadRegister
  | MountRegister
  | UnmountRegister
  | SubmitRegisterForm
  | SetRegisterFormUsername String
  | SetRegisterFormEmail String
  | SetRegisterFormPassword String
  | Noop

instance appViewEventShow :: Show AppViewEvent where
  show = case _ of
    SetPathString x -> "SetPathString: " <> x
    PreloadHome -> "PreloadHome"
    MountHome -> "MountHome"
    UnmountHome -> "UnmountHome"
    PreloadPosts -> "PreloadPosts"
    MountPosts -> "MountPosts"
    UnmountPosts -> "UnmountPosts"
    PreloadLogin -> "PreloadLogin"
    MountLogin -> "MountLogin"
    UnmountLogin -> "UnmountLogin"
    SubmitLoginForm -> "SubmitLoginForm"
    SetLoginFormEmail _ -> "SetLoginFormEmail"
    SetLoginFormPassword _ -> "SetLoginFormPassword"
    PreloadRegister -> "PreloadRegister"
    MountRegister -> "MountRegister"
    UnmountRegister -> "UnmountRegister"
    SubmitRegisterForm -> "SubmitRegisterForm"
    SetRegisterFormUsername _ -> "SetRegisterFormUsername"
    SetRegisterFormEmail _ -> "SetRegisterFormEmail"
    SetRegisterFormPassword _ -> "SetRegisterFormPassword"
    Noop -> "Noop"

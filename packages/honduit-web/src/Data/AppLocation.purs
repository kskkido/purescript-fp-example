module Data.AppLocation where

import Prelude
import Data.Generic.Rep as Generic

data AppLocation =
    Home
  | Posts
  | Login
  | Register
  | NotFound

instance appLocationShow :: Show AppLocation where
  show Home = "home"
  show Posts = "posts"
  show Login = "login"
  show Register = "register"
  show NotFound = "notFound"
derive instance appLocationGeneric :: Generic.Generic AppLocation _
derive instance appLocationEq :: Eq AppLocation

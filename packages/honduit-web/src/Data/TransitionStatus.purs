module Data.TransitionStatus where

import Prelude
import Data.Generic.Rep as Generic

data TransitionStatus =
    Mounting
  | Mounted
  | Unmounting
  | Unmounted

derive instance transitionStatusGeneric :: Generic.Generic TransitionStatus _
derive instance transitionStatusEq :: Eq TransitionStatus

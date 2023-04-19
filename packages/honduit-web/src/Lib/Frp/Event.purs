module Lib.Frp.Event where

import Prelude

type Event a =
  { time :: Number
  , value :: a
  }

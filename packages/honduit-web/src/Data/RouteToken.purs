module Data.RouteToken where

import Prelude

data RouteToken =
    Literal String
  | Dynamic String

module HonduitWebApi.Lib.String
  ( toInt
  ) where

import RIO

toInt :: String -> Maybe Int
toInt = readMaybe


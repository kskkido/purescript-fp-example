module Data.Path where

import Prelude
import Data.String.CodeUnits as String.CodeUnits
import Data.Array as Array
import Data.List as List
import Lib.List as List

type Path = List.List String

fromString :: String -> Path
fromString cs =
  ( ( List.fromFoldable ( String.CodeUnits.toCharArray cs ) ) #
    ( List.splitAt ((/=) '/') ) <#>
    ( List.toUnfoldable >>> String.CodeUnits.fromCharArray ) #
    ( List.filter ((/=) "" ) )
  )

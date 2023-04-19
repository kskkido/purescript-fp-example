module Data.PathValidator where

import Prelude
import Effect.Exception as Exception
import Control.Alt as Alt
import Control.Alternative as Alternative
import Data.Map as Map
import Data.Newtype as Newtype
import Data.List as List
import Data.Validator as Validator
import Data.Route as Route
import Data.Path as Path
import Data.PathParameters as PathParameters

type PathValidator a b = Validator.Validator a (List.List Exception.Error) b

match :: Route.Route -> PathValidator Path.Path PathParameters.PathParameters
match route = Newtype.wrap $ \paths -> ado
  Alternative.guard $ Route.match paths route
  in Route.toParams paths route

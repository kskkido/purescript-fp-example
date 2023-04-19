module Data.Route where

import Prelude
import Data.Map as Map
import Data.List as List
import Data.Maybe as Maybe
import Data.Tuple as Tuple
import Data.Foldable as Foldable
import Data.RouteToken as RouteToken
import Data.Validation as Validation
import Data.Path as Path

type Route = List.List RouteToken.RouteToken

match :: Path.Path -> Route -> Boolean
match path route =
  ( ( List.zipWith
      ( \ token -> case _ of
          RouteToken.Literal x -> token == x
          RouteToken.Dynamic _ -> true
      )
      ( path )
      ( route )
    ) <>
    ( pure $ List.length path == List.length route ) #
    ( Foldable.all identity )
  )

toParams :: Path.Path -> Route -> Map.Map String String
toParams path route =
  ( ( List.zipWith
      ( \ token -> case _ of
          RouteToken.Literal _ -> Maybe.Nothing
          RouteToken.Dynamic x -> pure (Tuple.Tuple token x)
      )
      ( path )
      ( route )
    ) #
    ( List.catMaybes ) #
    ( Map.fromFoldable )
  )


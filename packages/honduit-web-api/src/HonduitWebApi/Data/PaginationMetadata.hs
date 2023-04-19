module HonduitWebApi.Data.PaginationMetadata
  ( PaginationMetadata(..)
  , fromPaginatedResponse
  , toPagination
  ) where

import RIO
import qualified RIO.List as List
import qualified Data.Ord as Ord
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as Foldable
import qualified HonduitWebApi.Data.PaginatedResponse as PaginatedResponse

data PaginationMetadata = PaginationMetadata
  { page :: Int
  , pageSize :: Int
  , pageCount :: Int
  , itemCount :: Int
  }
  deriving (Generic, Show, Eq)

fromPaginatedResponse :: PaginatedResponse.PaginatedResponse a -> PaginationMetadata
fromPaginatedResponse response =
  PaginationMetadata
    response.page
    response.pageSize
    response.pageCount
    response.itemCount

toPagination :: Int -> Int -> PaginationMetadata -> [[Int]]
toPagination len lookaround metadata =
  Maybe.fromMaybe [] do
    let edge = lookaround + 1
        head = 1
        last = max head metadata.pageCount
        page = Ord.clamp (head, last) metadata.page
        step = \start _ acc -> Maybe.fromMaybe [start] do
                  prev <- List.lastMaybe acc
                  pure $ acc ++ [prev + 1]
    Foldable.asum
      [ do
          guard (last <= len)
          pure $
            [ foldr
                (step head)
                []
                (List.replicate (min len last) Nothing)
            ]
      , do
          guard (page <= edge || page >= last - edge)
          pure $
            [ foldr
                (step head)
                []
                (List.replicate edge Nothing)
            ] ++
            [ foldr
                (step (last - edge))
                []
                (List.replicate edge Nothing)
            ]
      , do
          pure $
            [ [head]
            ] ++
            [ foldr
                (step (page - lookaround))
                []
                (List.replicate (lookaround * 2 + 1) Nothing)
            ] ++
            [ [last]
            ]
      ]

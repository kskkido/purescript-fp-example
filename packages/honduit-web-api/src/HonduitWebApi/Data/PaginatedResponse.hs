module HonduitWebApi.Data.PaginatedResponse
  ( PaginatedResponse(..)
  ) where

import RIO

data PaginatedResponse a = PaginatedResponse
  { item :: a
  , itemCount :: Int
  , itemOffset :: Int
  , page :: Int
  , pageSize :: Int
  , pageCount :: Int
  }


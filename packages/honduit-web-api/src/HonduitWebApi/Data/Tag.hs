module HonduitWebApi.Data.Tag
  ( Tag(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Simple as PostgreSQL

data Tag = Tag
  { id :: Int
  , label :: String
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)

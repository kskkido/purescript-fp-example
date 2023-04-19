module HonduitDatabase.Data.ArticleTag
  ( ArticleTag(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Simple as PostgreSQL

data ArticleTag = ArticleTag
  { id :: Int
  , article_id :: Int
  , tag_id :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)


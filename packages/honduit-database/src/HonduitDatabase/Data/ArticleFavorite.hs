module HonduitDatabase.Data.ArticleFavorite
  ( ArticleFavorite(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time.Clock
import qualified Database.PostgreSQL.Simple as PostgreSQL

data ArticleFavorite = ArticleFavorite
  { id :: Int
  , article_id :: Int
  , user_profile_id :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)


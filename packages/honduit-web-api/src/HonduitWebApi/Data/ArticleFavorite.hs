module HonduitWebApi.Data.ArticleFavorite
  ( ArticleFavorite(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time.Clock
import qualified Database.PostgreSQL.Simple as PostgreSQL

data ArticleFavorite = ArticleFavorite
  { id :: Int
  , articleId :: Int
  , userProfileId :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)


module HonduitWebApi.Data.ArticleSlug
  ( ArticleSlug(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time.Clock
import qualified Database.PostgreSQL.Simple as PostgreSQL

data ArticleSlug = ArticleSlug
  { slug :: String
  , articleId :: Int
  , createdAt :: Time.Clock.UTCTime
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)


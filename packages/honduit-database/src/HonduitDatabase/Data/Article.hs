module HonduitDatabase.Data.Article
  ( Article(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time.Clock
import qualified Database.PostgreSQL.Simple as PostgreSQL

data Article = Article
  { slug :: String
  , id :: Int
  , title :: String
  , description :: String
  , body :: String
  , created_at :: Time.Clock.UTCTime
  , updated_at :: Time.Clock.UTCTime
  , author_id :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)

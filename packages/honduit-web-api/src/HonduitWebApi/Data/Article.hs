module HonduitWebApi.Data.Article
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
  , createdAt :: Time.Clock.UTCTime
  , updatedAt :: Time.Clock.UTCTime
  , authorId :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)

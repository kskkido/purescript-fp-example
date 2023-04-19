module HonduitCore.Data.Article
  ( Article(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time.Clock
import qualified HonduitCore.Data.ArticleSlug as ArticleSlug

data Article = Article
  { id :: Integer
  , slug :: ArticleSlug.ArticleSlug
  , title :: String
  , description :: String
  , body :: String
  , tagKeys :: [String]
  , createdAt :: Time.Clock.UTCTime
  , updatedAt :: Time.Clock.UTCTime
  , favoritesCount :: Int
  , authorId :: Integer
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)


module HonduitCore.Data.ArticleSlug
  ( ArticleSlug(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time.Clock

data ArticleSlug = ArticleSlug
  { key :: String
  , articleId :: Integer
  , createdAt :: Time.Clock.UTCTime
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

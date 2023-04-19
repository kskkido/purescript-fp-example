module HonduitCore.Data.ArticleFilter
  ( ArticleFilter(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time.Clock

data ArticleFilter = ArticleFilter
  { tagKey :: Maybe String
  , authorId :: Maybe Integer
  , favorited :: Maybe Bool
  , limit :: Maybe Int
  , offset :: Maybe Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

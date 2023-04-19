module HonduitCore.Data.AuthUserArticleRelation
  ( AuthUserArticleRelation(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time.Clock
import qualified HonduitCore.Data.ArticleSlug as ArticleSlug

data AuthUserArticleRelation = AuthUserArticleRelation
  { articleId :: Integer
  , articleSlug :: ArticleSlug.ArticleSlug
  , favoritedArticle :: Bool
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

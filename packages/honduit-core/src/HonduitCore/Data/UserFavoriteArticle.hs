module HonduitCore.Data.UserFavoriteArticle
  ( UserFavoriteArticle(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson

data UserFavoriteArticle = UserFavoriteArticle
  { id :: Integer
  , userId :: Integer
  , articleId :: Integer
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

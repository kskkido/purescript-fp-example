module HonduitWebApi.Data.AuthUserArticle
  ( AuthUserArticle(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time.Clock
import qualified HonduitWebApi.Data.Tag as Tag
import qualified HonduitWebApi.Data.AuthUserUserProfile as AuthUserUserProfile

data AuthUserArticle = AuthUserArticle
  { slug :: String
  , title :: String
  , description :: String
  , body :: String
  , tags :: [Tag.Tag]
  , createdAt :: Time.Clock.UTCTime
  , updatedAt :: Time.Clock.UTCTime
  , favorited :: Bool
  , favoritesCount :: Int
  , author :: AuthUserUserProfile.AuthUserUserProfile
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

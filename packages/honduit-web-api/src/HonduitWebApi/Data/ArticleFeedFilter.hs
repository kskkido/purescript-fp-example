module HonduitWebApi.Data.ArticleFeedFilter
  ( ArticleFeedFilter(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson

data ArticleFeedFilter = ArticleFeedFilter
  { tag :: Maybe String
  , authorId :: Maybe Int
  , favoritedUserProfileId :: Maybe Int
  , limit :: Int
  , offset :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)


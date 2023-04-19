module HonduitCore.Data.ArticleFeedFilter
  ( ArticleFeedFilter(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson

data ArticleFeedFilter = ArticleFeedFilter
  { tag :: Maybe String
  , authorId :: Maybe Integer
  , favoritedUserProfileId :: Maybe Integer
  , limit :: Maybe Int
  , offset :: Maybe Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

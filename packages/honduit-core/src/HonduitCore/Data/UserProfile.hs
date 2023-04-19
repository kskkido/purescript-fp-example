module HonduitCore.Data.UserProfile
  ( UserProfile(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson

data UserProfile = UserProfile
  { id :: Integer
  , username :: String
  , bio :: Maybe String
  , image :: Maybe String
  , followerUserProfileIds :: [Integer]
  , followingUserProfileIds :: [Integer]
  , favoriteArticleIds :: [Integer]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)


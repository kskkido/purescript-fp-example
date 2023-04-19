module HonduitCore.Data.UserProfileFollower
  ( UserProfileFollower(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson

data UserProfileFollower = UserProfileFollower
  { id :: Integer
  , followerUserProfileId :: Integer
  , followingUserProfileId :: Integer
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)


module HonduitDatabase.Data.UserProfileFollower
  ( UserProfileFollower(..)
  ) where

import RIO
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson

data UserProfileFollower = UserProfileFollower
  { follower_id :: Int
  , following_id :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)



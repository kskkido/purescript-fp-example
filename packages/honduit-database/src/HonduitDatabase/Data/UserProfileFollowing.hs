module HonduitDatabase.Data.UserProfileFollowing
  ( UserProfileFollowing(..)
  ) where

import RIO
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Simple as PostgreSQL

data UserProfileFollowing = UserProfileFollowing
  { follower_id :: Int
  , following_id :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.ToRow, PostgreSQL.FromRow)



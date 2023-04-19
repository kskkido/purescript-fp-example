module HonduitWebApi.Data.UserProfileFollower
  ( UserProfileFollower(..)
  ) where

import RIO
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Simple as PostgreSQL

data UserProfileFollower = UserProfileFollower
  { followerId :: Int
  , followingId :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.ToRow, PostgreSQL.FromRow)



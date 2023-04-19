module HonduitWebApi.Data.UserProfile
  ( UserProfile(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Simple as PostgreSQL

data UserProfile = UserProfile
  { id :: Int
  , userId :: Int
  , username :: String
  , bio :: Maybe String
  , image :: Maybe String
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.ToRow, PostgreSQL.FromRow)


module HonduitDatabase.Data.UserAuthCredential
  ( UserAuthCredential(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Simple as PostgreSQL

data UserAuthCredential = UserAuthCredential
  { id :: Int
  , user_id :: Int
  , email :: String
  , password :: String
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)


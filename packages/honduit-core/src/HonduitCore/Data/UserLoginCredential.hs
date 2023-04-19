module HonduitCore.Data.UserLoginCredential
  ( UserLoginCredential(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Simple as PostgreSQL

data UserLoginCredential = UserLoginCredential
  { id :: Integer
  , userId :: Integer
  , email :: String
  , password :: String
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.ToRow)

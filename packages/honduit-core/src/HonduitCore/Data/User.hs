module HonduitCore.Data.User
  ( User(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Simple as PostgreSQL

data User = User
  { id :: Integer
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

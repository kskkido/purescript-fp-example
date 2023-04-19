module HonduitCore.Data.UserProfileUpdate
  ( UserProfileUpdate(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson

data UserProfileUpdate = UserProfileUpdate
  { username :: Maybe String
  , bio :: Maybe String
  , image :: Maybe String
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

module HonduitWebApi.Data.UserLoginApiResponse
  ( UserLoginApiResponse(..)
  , fromUser
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified HonduitWebApi.Data.User as User

data UserLoginApiResponse = UserLoginApiResponse
  { user :: User.User
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

fromUser :: User.User -> UserLoginApiResponse
fromUser user = UserLoginApiResponse user

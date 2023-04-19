module HonduitWebApi.Data.UserLoginApiRequestBody
  ( UserLoginApiRequestBody
  , toUserLoginCredential
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified HonduitWebApi.Data.UserLoginCredential as UserLoginCredential

data UserLoginApiRequestBody = UserLoginApiRequestBody
  { user :: UserLoginApiRequestBodyUser
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data UserLoginApiRequestBodyUser = UserLoginApiRequestBodyUser
  { email :: String
  , password :: String
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

toUserLoginCredential :: UserLoginApiRequestBody -> UserLoginCredential.UserLoginCredential
toUserLoginCredential body =
  UserLoginCredential.UserLoginCredential
    body.user.email
    body.user.password

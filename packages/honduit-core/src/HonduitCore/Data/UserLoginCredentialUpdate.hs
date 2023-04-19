module HonduitCore.Data.UserLoginCredentialUpdate
  ( UserLoginCredentialUpdate(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson

data UserLoginCredentialUpdate = UserLoginCredentialUpdate
  { email :: Maybe String
  , password :: Maybe String
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

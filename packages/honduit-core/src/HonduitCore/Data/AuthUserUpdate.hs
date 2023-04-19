module HonduitCore.Data.AuthUserUpdate
  ( AuthUserUpdate(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson

data AuthUserUpdate = AuthUserUpdate
  { email :: Maybe String
  , username :: Maybe String
  , password :: Maybe String
  , bio :: Maybe String
  , image :: Maybe String
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)


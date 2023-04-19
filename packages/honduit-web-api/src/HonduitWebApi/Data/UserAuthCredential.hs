module HonduitWebApi.Data.UserAuthCredential
  ( UserAuthCredential(..)
  , fromDatabaseSchema
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified HonduitDatabase.Data.UserAuthCredential as HonduitDatabase.UserAuthCredential

data UserAuthCredential = UserAuthCredential
  { id :: Int
  , userId :: Int
  , email :: String
  , password :: String
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

fromDatabaseSchema :: HonduitDatabase.UserAuthCredential.UserAuthCredential -> UserAuthCredential
fromDatabaseSchema schema =
  UserAuthCredential
    schema.id
    schema.user_id
    schema.email
    schema.password

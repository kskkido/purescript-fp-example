module HonduitWebApi.Data.UserLoginCredential
  ( UserLoginCredential(..)
  , fromBasicAuthData
  ) where

import RIO
import qualified Servant.API
import qualified Data.Aeson as Aeson
import qualified HonduitWebApi.Lib.ByteString as ByteString
import qualified Database.PostgreSQL.Simple as PostgreSQL

data UserLoginCredential = UserLoginCredential
  { email :: String
  , password :: String
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.ToRow)

fromBasicAuthData :: Servant.API.BasicAuthData -> UserLoginCredential
fromBasicAuthData basicAuthData =
  UserLoginCredential
    ( Servant.API.basicAuthUsername basicAuthData & ByteString.toString )
    ( Servant.API.basicAuthPassword basicAuthData & ByteString.toString )

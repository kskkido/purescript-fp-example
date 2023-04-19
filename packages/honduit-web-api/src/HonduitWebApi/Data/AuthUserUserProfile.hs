module HonduitWebApi.Data.AuthUserUserProfile
  ( AuthUserUserProfile(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson

data AuthUserUserProfile = AuthUserUserProfile
  { id :: Int
  , username :: String
  , bio :: Maybe String
  , image :: Maybe String
  , following :: Bool
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)


module HonduitWebApi.Data.AuthUser
  ( AuthUser(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson

data AuthUser = AuthUser
  { userId :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)


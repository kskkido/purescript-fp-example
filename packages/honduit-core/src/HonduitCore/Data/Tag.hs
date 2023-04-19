module HonduitCore.Data.Tag
  ( Tag(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson

data Tag = Tag
  { key :: String
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

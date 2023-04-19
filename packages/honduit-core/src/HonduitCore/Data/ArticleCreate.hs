module HonduitCore.Data.ArticleCreate
  ( ArticleCreate(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson

data ArticleCreate = ArticleCreate
  { title :: String
  , description :: String
  , body :: String
  , tagKeys :: Maybe [String]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

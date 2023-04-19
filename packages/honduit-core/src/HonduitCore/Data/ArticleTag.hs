module HonduitCore.Data.ArticleTag
  ( ArticleTag(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson

data ArticleTag = ArticleTag
  { key :: String
  , articleIds :: [Integer]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

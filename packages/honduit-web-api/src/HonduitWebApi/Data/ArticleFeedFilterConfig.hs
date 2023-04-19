module HonduitWebApi.Data.ArticleFeedFilterConfig
  ( ArticleFeedFilterConfig(..)
  , HasArticleFeedFilterConfig(..)
  ) where

import RIO

data ArticleFeedFilterConfig = ArticleFeedFilterConfig
  { defaultArticleFeedLimit :: Int
  , defaultArticleFeedOffset :: Int
  }
  deriving (Eq, Show)

class HasArticleFeedFilterConfig a where
  get :: a -> ArticleFeedFilterConfig
instance HasArticleFeedFilterConfig ArticleFeedFilterConfig where
  get = id

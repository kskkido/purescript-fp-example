module HonduitCore.Data.AuthUserArticleCommentRelation
  ( AuthUserArticleCommentRelation(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time.Clock

data AuthUserArticleCommentRelation = AuthUserArticleCommentRelation
  { commentId :: Integer
  , likedComment :: Bool
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)


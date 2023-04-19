module HonduitCore.Data.AuthUserUserProfileRelation
  ( AuthUserUserProfileRelation(..)
  ) where

import RIO
import qualified Data.Aeson as Aeson

data AuthUserUserProfileRelation = AuthUserUserProfileRelation
  { userProfileId :: Integer
  , followedUserProfile :: Bool
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)


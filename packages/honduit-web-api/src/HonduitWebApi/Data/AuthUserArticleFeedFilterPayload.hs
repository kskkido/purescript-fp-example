module HonduitWebApi.Data.AuthUserArticleFeedFilterPayload
  ( AuthUserArticleFeedFilterPayload(..)
  , fromQuery
  , toQueryString
  ) where

import RIO
import qualified RIO.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Aeson as Aeson

data AuthUserArticleFeedFilterPayload = AuthUserArticleFeedFilterPayload
  { tag :: Maybe String
  , authorId :: Maybe Int
  , favorited :: Maybe Bool
  , limit :: Maybe Int
  , offset :: Maybe Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

fromQuery :: (AuthUserArticleFeedFilterPayload -> m a) -> Maybe String -> Maybe Int -> Maybe Bool -> Maybe Int -> Maybe Int -> m a
fromQuery fn tag authorId favorited limit offset =
  fn $
    AuthUserArticleFeedFilterPayload
      tag
      authorId
      favorited
      limit
      offset

toQueryString :: AuthUserArticleFeedFilterPayload -> String
toQueryString feedFilter =
  List.intercalate
    "&"
    ( Maybe.catMaybes
        [ do
            value <- show <$> feedFilter.tag
            pure $ "tag=" <> value
        , do
            value <- show <$> feedFilter.authorId
            pure $ "authorId=" <> value
        , do
            value <- show <$> feedFilter.favorited
            pure $ "favorited=" <> value
        , do
            value <- show <$> feedFilter.limit
            pure $ "limit=" <> value
        , do
            value <- show <$> feedFilter.offset
            pure $ "offset=" <> value
        ]
    )

module HonduitWebApi.Data.AuthUserArticleFeedFilter
  ( AuthUserArticleFeedFilter(..)
  , fromPayload
  , toPayload
  , toQueryString
  , toArticleFeedFilter
  , toArticleFeedFilterByAuthUser
  ) where

import RIO
import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified HonduitWebApi.Data.AuthUser as AuthUser
import qualified HonduitWebApi.Data.ArticleFeedFilterConfig as ArticleFeedFilterConfig
import qualified HonduitWebApi.Data.AuthUserArticleFeedFilterPayload as AuthUserArticleFeedFilterPayload
import qualified HonduitWebApi.Data.ArticleFeedFilter as ArticleFeedFilter

data AuthUserArticleFeedFilter = AuthUserArticleFeedFilter
  { tag :: Maybe String
  , authorId :: Maybe Int
  , favorited :: Maybe Bool
  , limit :: Int
  , offset :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

fromPayload :: (ArticleFeedFilterConfig.HasArticleFeedFilterConfig e, Monad m) => AuthUserArticleFeedFilterPayload.AuthUserArticleFeedFilterPayload -> Reader.ReaderT e m AuthUserArticleFeedFilter
fromPayload queryParameters = do
  context <- Reader.asks ArticleFeedFilterConfig.get
  pure $ AuthUserArticleFeedFilter
    queryParameters.tag
    queryParameters.authorId
    queryParameters.favorited
    ( fromMaybe context.defaultArticleFeedLimit queryParameters.limit )
    ( fromMaybe context.defaultArticleFeedOffset queryParameters.offset )

toPayload :: AuthUserArticleFeedFilter -> AuthUserArticleFeedFilterPayload.AuthUserArticleFeedFilterPayload
toPayload feedFilter =
  AuthUserArticleFeedFilterPayload.AuthUserArticleFeedFilterPayload
    feedFilter.tag
    feedFilter.authorId
    feedFilter.favorited
    ( pure feedFilter.limit )
    ( pure feedFilter.offset )

toQueryString :: AuthUserArticleFeedFilter -> String
toQueryString = AuthUserArticleFeedFilterPayload.toQueryString . toPayload

toArticleFeedFilter :: AuthUserArticleFeedFilter -> ArticleFeedFilter.ArticleFeedFilter
toArticleFeedFilter feedFilter =
  ArticleFeedFilter.ArticleFeedFilter
    feedFilter.tag
    feedFilter.authorId
    Nothing
    feedFilter.limit
    feedFilter.offset

toArticleFeedFilterByAuthUser :: AuthUser.AuthUser -> AuthUserArticleFeedFilter -> ArticleFeedFilter.ArticleFeedFilter
toArticleFeedFilterByAuthUser authUser feedFilter =
  ArticleFeedFilter.ArticleFeedFilter
    feedFilter.tag
    feedFilter.authorId
    ( do
        favorited <- feedFilter.favorited
        guard favorited
        pure $ authUser.userId
    )
    feedFilter.limit
    feedFilter.offset


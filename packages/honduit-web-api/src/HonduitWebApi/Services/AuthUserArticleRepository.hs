module HonduitWebApi.Services.AuthUserArticleRepository
  ( toInterface
  ) where

import RIO
import qualified System.IO as IO
import qualified Control.Monad.Reader as Reader
import qualified HonduitWebApi.Data.PaginatedResponse as PaginatedResponse
import qualified HonduitWebApi.Data.AuthUser as AuthUser
import qualified HonduitWebApi.Data.AuthUserArticle as AuthUserArticle
import qualified HonduitWebApi.Data.AuthUserArticleFeedFilter as AuthUserArticleFeedFilter
import qualified HonduitWebApi.Data.AuthUserArticleRepositoryContext as AuthUserArticleRepositoryContext
import qualified HonduitWebApi.Data.AuthUserArticleRepositoryInterface as AuthUserArticleRepositoryInterface
import qualified HonduitWebApi.Data.AppDatabaseOrmContext as AppDatabaseOrmContext
import qualified HonduitWebApi.Services.AppDatabaseOrm as AppDatabaseOrm

type AuthUserArticleRepository m a = Reader.ReaderT AuthUserArticleRepositoryContext.AuthUserArticleRepositoryContext m a

toInterface :: AuthUserArticleRepositoryContext.AuthUserArticleRepositoryContext -> AuthUserArticleRepositoryInterface.AuthUserArticleRepositoryInterface
toInterface context =
  AuthUserArticleRepositoryInterface.AuthUserArticleRepositoryInterface
    ( \feedFilter -> Reader.runReaderT ( getFeedByFilter feedFilter ) context )
    ( \feedFilter user -> Reader.runReaderT ( getFeedByFilterByAuthUser feedFilter user ) context )

getFeedByFilter :: AuthUserArticleFeedFilter.AuthUserArticleFeedFilter -> AuthUserArticleRepository IO.IO (PaginatedResponse.PaginatedResponse [AuthUserArticle.AuthUserArticle])
getFeedByFilter feedFilter = do
  Reader.withReaderT AppDatabaseOrmContext.get do
    articlesCount <- AppDatabaseOrm.getArticlesCountByFilter $ AuthUserArticleFeedFilter.toArticleFeedFilter feedFilter
    articles <- AppDatabaseOrm.getAuthUserArticlesByFilter feedFilter
    pure $ PaginatedResponse.PaginatedResponse
      articles
      articlesCount
      feedFilter.offset
      (ceiling $ (fromIntegral feedFilter.offset / fromIntegral feedFilter.limit :: Double))
      feedFilter.limit
      (ceiling $ (fromIntegral articlesCount / fromIntegral feedFilter.limit :: Double))

getFeedByFilterByAuthUser :: AuthUserArticleFeedFilter.AuthUserArticleFeedFilter -> AuthUser.AuthUser -> AuthUserArticleRepository IO.IO (PaginatedResponse.PaginatedResponse [AuthUserArticle.AuthUserArticle])
getFeedByFilterByAuthUser feedFilter authUser = do
  Reader.withReaderT AppDatabaseOrmContext.get do
    articlesCount <- AppDatabaseOrm.getArticlesCountByFilter $ AuthUserArticleFeedFilter.toArticleFeedFilterByAuthUser authUser feedFilter
    articles <- AppDatabaseOrm.getAuthUserArticlesByFilterByAuthUser feedFilter authUser
    pure $ PaginatedResponse.PaginatedResponse
      articles
      articlesCount
      feedFilter.offset
      (ceiling $ (fromIntegral feedFilter.offset / fromIntegral feedFilter.limit :: Double))
      feedFilter.limit
      (ceiling $ (fromIntegral articlesCount / fromIntegral feedFilter.limit :: Double))

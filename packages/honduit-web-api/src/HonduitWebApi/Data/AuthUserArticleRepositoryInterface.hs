module HonduitWebApi.Data.AuthUserArticleRepositoryInterface
  ( AuthUserArticleRepositoryInterface(..)
  ) where

import qualified System.IO as IO
import qualified HonduitWebApi.Data.PaginatedResponse as PaginatedResponse
import qualified HonduitWebApi.Data.AuthUser as AuthUser
import qualified HonduitWebApi.Data.AuthUserArticle as AuthUserArticle
import qualified HonduitWebApi.Data.AuthUserArticleFeedFilter as AuthUserArticleFeedFilter

data AuthUserArticleRepositoryInterface = AuthUserArticleRepositoryInterface
  { getFeedByFilter :: AuthUserArticleFeedFilter.AuthUserArticleFeedFilter -> IO.IO (PaginatedResponse.PaginatedResponse [AuthUserArticle.AuthUserArticle])
  , getFeedByFilterByUser :: AuthUserArticleFeedFilter.AuthUserArticleFeedFilter -> AuthUser.AuthUser -> IO.IO (PaginatedResponse.PaginatedResponse [AuthUserArticle.AuthUserArticle])
  }

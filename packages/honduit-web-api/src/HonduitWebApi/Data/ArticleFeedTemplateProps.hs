module HonduitWebApi.Data.ArticleFeedTemplateProps
  ( ArticleFeedTemplateProps(..)
  ) where

import RIO
import qualified HonduitWebApi.Data.PaginatedResponse as PaginatedResponse
import qualified HonduitWebApi.Data.AuthUserArticle as AuthUserArticle
import qualified HonduitWebApi.Data.AuthUserArticleFeedFilter as AuthUserArticleFeedFilter

data ArticleFeedTemplateProps = ArticleFeedTemplateProps
  { filter :: AuthUserArticleFeedFilter.AuthUserArticleFeedFilter
  , articles :: PaginatedResponse.PaginatedResponse [AuthUserArticle.AuthUserArticle]
  }


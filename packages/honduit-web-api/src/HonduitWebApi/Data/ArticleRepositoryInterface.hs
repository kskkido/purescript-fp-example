module HonduitWebApi.Data.ArticleRepositoryInterface
  ( ArticleRepositoryInterface(..)
  ) where

import qualified System.IO as IO
import qualified Data.Maybe as Maybe
import qualified HonduitWebApi.Data.Article as Article
import qualified HonduitWebApi.Data.ArticleLoginCredential as ArticleLoginCredential

data ArticleRepositoryInterface = ArticleRepositoryInterface
  { findArticleByLoginCredential :: ArticleLoginCredential.ArticleLoginCredential -> IO.IO (Maybe.Maybe Article.Article)
  }

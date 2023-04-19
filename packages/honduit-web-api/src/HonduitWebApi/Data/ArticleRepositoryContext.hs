module HonduitWebApi.Data.ArticleRepositoryContext
  ( ArticleRepositoryContext(..)
  , toDatabaseOrmContext
  ) where

import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified HonduitWebApi.Data.AppDatabaseOrmContext as AppDatabaseOrmContext

data ArticleRepositoryContext = ArticleRepositoryContext
  { database :: PostgreSQL.Connection
  }

instance AppDatabaseOrmContext.HasAppDatabaseOrmContext ArticleRepositoryContext where
  get = toDatabaseOrmContext

toDatabaseOrmContext :: ArticleRepositoryContext -> AppDatabaseOrmContext.AppDatabaseOrmContext
toDatabaseOrmContext context =
  AppDatabaseOrmContext.AppDatabaseOrmContext context.database

module HonduitWebApi.Data.AuthUserArticleRepositoryContext
  ( AuthUserArticleRepositoryContext(..)
  ) where

import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified HonduitWebApi.Data.AppDatabaseOrmContext as AppDatabaseOrmContext

data AuthUserArticleRepositoryContext = AuthUserArticleRepositoryContext
  { database :: PostgreSQL.Connection
  }

instance AppDatabaseOrmContext.HasAppDatabaseOrmContext AuthUserArticleRepositoryContext where
  get = toDatabaseOrmContext

toDatabaseOrmContext :: AuthUserArticleRepositoryContext -> AppDatabaseOrmContext.AppDatabaseOrmContext
toDatabaseOrmContext context =
  AppDatabaseOrmContext.AppDatabaseOrmContext context.database

module HonduitWebApi.Data.UserRepositoryContext
  ( UserRepositoryContext(..)
  , toDatabaseOrmContext
  ) where

import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified HonduitWebApi.Data.AppDatabaseOrmContext as AppDatabaseOrmContext

data UserRepositoryContext = UserRepositoryContext
  { database :: PostgreSQL.Connection
  }

instance AppDatabaseOrmContext.HasAppDatabaseOrmContext UserRepositoryContext where
  get = toDatabaseOrmContext

toDatabaseOrmContext :: UserRepositoryContext -> AppDatabaseOrmContext.AppDatabaseOrmContext
toDatabaseOrmContext context =
  AppDatabaseOrmContext.AppDatabaseOrmContext context.database

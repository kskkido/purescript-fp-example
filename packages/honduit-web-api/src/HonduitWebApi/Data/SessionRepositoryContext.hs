module HonduitWebApi.Data.SessionRepositoryContext
  ( SessionRepositoryContext(..)
  , toDatabaseOrmContext
  ) where

import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified HonduitWebApi.Data.AppDatabaseOrmContext as AppDatabaseOrmContext

data SessionRepositoryContext = SessionRepositoryContext
  { database :: PostgreSQL.Connection
  }

instance AppDatabaseOrmContext.HasAppDatabaseOrmContext SessionRepositoryContext where
  get = toDatabaseOrmContext

toDatabaseOrmContext :: SessionRepositoryContext -> AppDatabaseOrmContext.AppDatabaseOrmContext
toDatabaseOrmContext context =
  AppDatabaseOrmContext.AppDatabaseOrmContext context.database

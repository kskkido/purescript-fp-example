module HonduitWebApi.Data.AppDatabaseOrmContext
  ( AppDatabaseOrmContext(..)
  , HasAppDatabaseOrmContext(..)
  ) where

import RIO
import qualified Database.PostgreSQL.Simple as PostgreSQL

data AppDatabaseOrmContext = AppDatabaseOrmContext
  { database :: PostgreSQL.Connection
  }

class HasAppDatabaseOrmContext a where
  get :: a -> AppDatabaseOrmContext
instance HasAppDatabaseOrmContext AppDatabaseOrmContext where
  get = id

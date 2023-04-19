module HonduitDatabase.Data.AppContext
  ( AppContext(..)
  , HasAppContext(..)
  , fromAppConfig
  ) where

import RIO
import qualified System.IO as IO
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified HonduitDatabase.Data.AppConfig as AppConfig

data AppContext = AppContext
  { appConfig :: AppConfig.AppConfig
  , database :: PostgreSQL.Connection
  }

class HasAppContext a where
  get :: a -> AppContext
instance HasAppContext AppContext where
  get = id
instance AppConfig.HasAppConfig AppContext where
  get context = context.appConfig

fromAppConfig :: AppConfig.AppConfig -> IO.IO AppContext
fromAppConfig appConfig = do
  database <- do
    let connectInfo = AppConfig.toPostgresConnectInfo appConfig
    PostgreSQL.connect connectInfo
  pure $
    AppContext
      appConfig
      database


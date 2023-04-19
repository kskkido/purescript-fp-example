module HonduitDatabase.Data.AppConfig
  ( AppConfig(..)
  , HasAppConfig(..)
  , fromSystem
  , toPostgresConnectInfo
  ) where

import RIO
import qualified RIO.Text as Text
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Data.Time.Clock as Time.Clock
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified HonduitDatabase.Lib.MaybeT as MaybeT
import qualified HonduitDatabase.Lib.ExceptT as ExceptT

data AppConfig = AppConfig
  { databaseHost :: Text
  , databasePort :: Word16
  , databaseUser :: String
  , databaseName :: String
  , databasePassword :: String
  } deriving (Eq, Show)

class HasAppConfig a where
  get :: a -> AppConfig
instance HasAppConfig AppConfig where
  get = id

fromSystem :: Except.ExceptT String IO.IO AppConfig
fromSystem = do
  databaseHost <- ExceptT.fromMaybeT "Invalid DATABASE_HOST" do
    Text.pack <$> Maybe.MaybeT (Environment.lookupEnv "DATABASE_HOST")
  databasePort <- ExceptT.fromMaybeT "Invalid DATABASE_PORT" do
    value <- Maybe.MaybeT (Environment.lookupEnv "DATABASE_PORT")
    MaybeT.fromMaybe (readMaybe value)
  databaseUser <- ExceptT.fromMaybeT "Invalid DATABASE_USER" do
    Maybe.MaybeT (Environment.lookupEnv "DATABASE_USER")
  databaseName <- ExceptT.fromMaybeT "Invalid DATABASE_NAME" do
    Maybe.MaybeT (Environment.lookupEnv "DATABASE_NAME")
  databasePassword <- ExceptT.fromMaybeT "Invalid DATABASE_PASSWORD" do
    Maybe.MaybeT (Environment.lookupEnv "DATABASE_PASSWORD")
  pure $
    AppConfig
      databaseHost
      databasePort
      databaseUser
      databaseName
      databasePassword

toPostgresConnectInfo :: HasAppConfig a => a -> PostgreSQL.ConnectInfo
toPostgresConnectInfo hasAppConfigConfig =
  let appConfig = get hasAppConfigConfig
   in PostgreSQL.ConnectInfo
        { PostgreSQL.connectHost = Text.unpack $ appConfig.databaseHost
        , PostgreSQL.connectPort = appConfig.databasePort
        , PostgreSQL.connectUser = appConfig.databaseUser
        , PostgreSQL.connectDatabase = appConfig.databaseName
        , PostgreSQL.connectPassword = appConfig.databasePassword
        }

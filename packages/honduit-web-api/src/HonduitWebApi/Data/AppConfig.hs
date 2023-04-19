module HonduitWebApi.Data.AppConfig
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
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified HonduitWebApi.Lib.MaybeT as MaybeT
import qualified HonduitWebApi.Lib.ExceptT as ExceptT
import qualified HonduitWebApi.Lib.ByteString as ByteString

data AppConfig = AppConfig
  { stage :: String
  , serverPort :: Int
  , databaseHost :: Text
  , databasePort :: Word16
  , databaseUser :: String
  , databaseName :: String
  , databasePassword :: String
  , authCookieName :: String
  , authCookieMaxAge :: Time.Clock.DiffTime
  , authJwtSecret :: ByteString.Lazy.ByteString
  , authJwtExpiresIn :: Time.Clock.NominalDiffTime
  , authJwtClientId :: String
  , publicAssetsFilePath :: String
  } deriving (Eq, Show)

class HasAppConfig a where
  get :: a -> AppConfig
instance HasAppConfig AppConfig where
  get = id

fromSystem :: Except.ExceptT String IO.IO AppConfig
fromSystem = do
  stage <- ExceptT.fromMaybeT "Invalid STAGE" do
    Maybe.MaybeT (Environment.lookupEnv "STAGE")
  serverPort <- ExceptT.fromMaybeT "Invalid SERVER_PORT" do
    value <- Maybe.MaybeT (Environment.lookupEnv "SERVER_PORT")
    MaybeT.fromMaybe (readMaybe value)
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
  authCookieName <- ExceptT.fromMaybeT "Invalid AUTH_COOKIE_NAME" do
    Maybe.MaybeT (Environment.lookupEnv "AUTH_COOKIE_NAME")
  authCookieMaxAge <- ExceptT.fromMaybeT "Invalid AUTH_COOKIE_MAX_AGE" do
    value <- Maybe.MaybeT (Environment.lookupEnv "AUTH_COOKIE_MAX_AGE")
    MaybeT.fromMaybe (fromInteger <$> readMaybe value)
  authJwtSecret <- ExceptT.fromMaybeT "Invalid AUTH_JWT_SECRET" do
    ByteString.toLazy . ByteString.fromUtf8String <$> Maybe.MaybeT (Environment.lookupEnv "AUTH_JWT_SECRET")
  authJwtExpiresIn <- ExceptT.fromMaybeT "Invalid AUTH_JWT_EXPIRES_IN" do
    value <- Maybe.MaybeT (Environment.lookupEnv "AUTH_JWT_EXPIRES_IN")
    MaybeT.fromMaybe (fromInteger <$> readMaybe value)
  authJwtClientId <- ExceptT.fromMaybeT "Invalid AUTH_JWT_CLIENT_ID" do
    Maybe.MaybeT (Environment.lookupEnv "AUTH_JWT_CLIENT_ID")
  publicAssetsFilePath <- ExceptT.fromMaybeT "Invalid PUBLIC_ASSETS_FILE_PATH" do
    Maybe.MaybeT (Environment.lookupEnv "PUBLIC_ASSETS_FILE_PATH")
  pure $
    AppConfig
      stage
      serverPort
      databaseHost
      databasePort
      databaseUser
      databaseName
      databasePassword
      authCookieName
      authCookieMaxAge
      authJwtSecret
      authJwtExpiresIn
      authJwtClientId
      publicAssetsFilePath

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

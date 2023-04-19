module HonduitWebApi.Data.AppContext
  ( AppContext(..)
  , HasAppContext(..)
  , fromAppConfig
  ) where

import RIO
import qualified System.IO as IO
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified HonduitWebApi.Data.AppConfig as AppConfig
import qualified HonduitWebApi.Data.AuthUserArticleRepositoryContext as AuthUserArticleRepositoryContext
import qualified HonduitWebApi.Data.AuthUserArticleRepositoryInterface as AuthUserArticleRepositoryInterface
import qualified HonduitWebApi.Data.UserRepositoryContext as UserRepositoryContext
import qualified HonduitWebApi.Data.UserRepositoryInterface as UserRepositoryInterface
import qualified HonduitWebApi.Data.AuthConfig as AuthConfig
import qualified HonduitWebApi.Services.AuthUserArticleRepository as AuthUserArticleRepository
import qualified HonduitWebApi.Services.UserRepository as UserRepository
import qualified HonduitWebApi.Services.SessionRepository as SessionRepository
import qualified HonduitWebApi.Data.ArticleFeedFilterConfig as ArticleFeedFilterConfig

data AppContext = AppContext
  { appConfig :: AppConfig.AppConfig
  , database :: PostgreSQL.Connection
  , userRepository :: UserRepositoryInterface.UserRepositoryInterface
  , authUserArticleRepository :: AuthUserArticleRepositoryInterface.AuthUserArticleRepositoryInterface
  , defaultArticleFeedLimit :: Int
  , defaultArticleFeedOffset :: Int
  }

class HasAppContext a where
  get :: a -> AppContext
instance HasAppContext AppContext where
  get = id
instance AppConfig.HasAppConfig AppContext where
  get context = context.appConfig
instance AuthConfig.HasAuthConfig AppContext where
  get context =
    AuthConfig.AuthConfig
      context.appConfig.authCookieName
      context.appConfig.authCookieMaxAge
      context.appConfig.authJwtSecret
      context.appConfig.authJwtClientId
      context.appConfig.authJwtExpiresIn
instance ArticleFeedFilterConfig.HasArticleFeedFilterConfig AppContext where
  get context =
    ArticleFeedFilterConfig.ArticleFeedFilterConfig
      context.defaultArticleFeedLimit
      context.defaultArticleFeedOffset

fromAppConfig :: AppConfig.AppConfig -> IO.IO AppContext
fromAppConfig appConfig = do
  database <- do
    let connectInfo = AppConfig.toPostgresConnectInfo appConfig
    PostgreSQL.connect connectInfo
  pure $
    AppContext
      appConfig
      database
      ( ( UserRepositoryContext.UserRepositoryContext database ) &
        ( UserRepository.toInterface )
      )
      ( ( AuthUserArticleRepositoryContext.AuthUserArticleRepositoryContext database ) &
        ( AuthUserArticleRepository.toInterface )
      )
      20
      0


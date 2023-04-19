module HonduitWebApi.Services.SessionRepository
  ( SessionRepository(..)
  , toInterface
  , login
  ) where

import RIO
import qualified System.IO as IO
import qualified Control.Monad.Reader as Reader
import qualified HonduitWebApi.Data.AuthUser as AuthUser
import qualified HonduitWebApi.Data.User as User
import qualified HonduitWebApi.Data.UserLoginCredential as UserLoginCredential
import qualified HonduitWebApi.Data.SessionRepositoryContext as SessionRepositoryContext
import qualified HonduitWebApi.Data.SessionRepositoryInterface as SessionRepositoryInterface
import qualified HonduitWebApi.Data.AppDatabaseOrmContext as AppDatabaseOrmContext
import qualified HonduitWebApi.Services.AppDatabaseOrm as AppDatabaseOrm

type SessionRepository m a = Reader.ReaderT SessionRepositoryContext.SessionRepositoryContext m a

toInterface :: SessionRepositoryContext.SessionRepositoryContext -> SessionRepositoryInterface.SessionRepositoryInterface
toInterface context =
  SessionRepositoryInterface.SessionRepositoryInterface
    ( \credential -> Reader.runReaderT ( login credential ) context )

login :: UserLoginCredential.UserLoginCredential -> SessionRepository IO.IO AuthUser.AuthUser
login credential = do
  Reader.withReaderT AppDatabaseOrmContext.get do
    user <- AppDatabaseOrm.getUserByLoginCredential credential
    pure $ AuthUser.AuthUser user.id

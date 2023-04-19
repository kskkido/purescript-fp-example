module HonduitWebApi.Services.UserRepository
  ( toInterface
  , findUserByLoginCredential
  ) where

import qualified System.IO as IO
import qualified Control.Monad.Reader as Reader
import qualified Data.Maybe as Maybe
import qualified HonduitWebApi.Data.User as User
import qualified HonduitWebApi.Data.UserLoginCredential as UserLoginCredential
import qualified HonduitWebApi.Data.UserRepositoryContext as UserRepositoryContext
import qualified HonduitWebApi.Data.UserRepositoryInterface as UserRepositoryInterface
import qualified HonduitWebApi.Data.AppDatabaseOrmContext as AppDatabaseOrmContext
import qualified HonduitWebApi.Services.AppDatabaseOrm as AppDatabaseOrm

type UserRepository m a = Reader.ReaderT UserRepositoryContext.UserRepositoryContext m a

toInterface :: UserRepositoryContext.UserRepositoryContext -> UserRepositoryInterface.UserRepositoryInterface
toInterface context =
  UserRepositoryInterface.UserRepositoryInterface
    ( \payload -> Reader.runReaderT ( getUserByLoginCredential payload ) context )
    ( \payload -> Reader.runReaderT ( findUserByLoginCredential payload ) context )

getUserByLoginCredential :: UserLoginCredential.UserLoginCredential -> UserRepository IO.IO User.User
getUserByLoginCredential payload = do
  Reader.withReaderT AppDatabaseOrmContext.get do
    AppDatabaseOrm.getUserByLoginCredential payload

findUserByLoginCredential :: UserLoginCredential.UserLoginCredential -> UserRepository IO.IO (Maybe.Maybe User.User)
findUserByLoginCredential payload = do
  Reader.withReaderT AppDatabaseOrmContext.get do
    AppDatabaseOrm.findUserByLoginCredential payload

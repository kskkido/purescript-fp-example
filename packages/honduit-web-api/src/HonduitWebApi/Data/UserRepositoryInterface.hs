module HonduitWebApi.Data.UserRepositoryInterface
  ( UserRepositoryInterface(..)
  ) where

import qualified System.IO as IO
import qualified Data.Maybe as Maybe
import qualified HonduitWebApi.Data.User as User
import qualified HonduitWebApi.Data.UserLoginCredential as UserLoginCredential

data UserRepositoryInterface = UserRepositoryInterface
  { getUserByLoginCredential :: UserLoginCredential.UserLoginCredential -> IO.IO User.User
  , findUserByLoginCredential :: UserLoginCredential.UserLoginCredential -> IO.IO (Maybe.Maybe User.User)
  }

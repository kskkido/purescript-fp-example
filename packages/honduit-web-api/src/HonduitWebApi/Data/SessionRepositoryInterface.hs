module HonduitWebApi.Data.SessionRepositoryInterface
  ( SessionRepositoryInterface(..)
  ) where

import RIO
import qualified System.IO as IO
import qualified HonduitWebApi.Data.AuthUser as AuthUser
import qualified HonduitWebApi.Data.UserLoginCredential as UserLoginCredential

data SessionRepositoryInterface = SessionRepositoryInterface
  { login :: UserLoginCredential.UserLoginCredential -> IO.IO AuthUser.AuthUser
  }


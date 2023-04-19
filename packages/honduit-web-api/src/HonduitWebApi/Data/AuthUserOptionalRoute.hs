module HonduitWebApi.Data.AuthUserOptionalRoute
  ( AuthUserOptionalRoute
  ) where

import RIO
import qualified Servant.API
import qualified HonduitWebApi.Data.AuthUser as AuthUser

type AuthUserOptionalRoute =
  ( ( Servant.API.BasicAuth "auth-user-optional" (Maybe AuthUser.AuthUser) )
  )

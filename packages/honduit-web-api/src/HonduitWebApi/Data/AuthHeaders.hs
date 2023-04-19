module HonduitWebApi.Data.AuthHeaders
  ( AuthHeaders
  ) where

import RIO
import qualified Servant.API
import qualified Web.Cookie as Cookie

type AuthHeaders =
 '[ Servant.API.Header "Set-Cookie" Cookie.SetCookie
  ]
